{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Test.Tasty.Req.Runner
  ( Error(..), WithCommand(..),
    OptionModifier,
    runCommands, responseParser
  ) where

import Control.Exception               (SomeException, catch, throwIO)
import Control.Lens                    (Prism', (#), prism', review)
import Control.Monad                   (unless)
import Control.Monad.Except            (MonadError, catchError, throwError)
import Control.Monad.IO.Class          (MonadIO)
import Control.Monad.Random            (RandT, liftRandT, runRandT)
import Control.Monad.Random.Class      (MonadRandom, getRandom)
import Control.Monad.State             (MonadState, gets, modify, state)
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Either      (EitherT, left, mapEitherT, runEitherT)
import Control.Monad.Trans.Reader      (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State       (StateT(StateT), runStateT)
import Data.Bifunctor                  (Bifunctor, bimap, first)
import Data.ByteString.Char8           (ByteString)
import Data.Default                    (def)
import Data.Functor.Identity           (Identity)
import Data.List.NonEmpty              (NonEmpty((:|)))
import Data.Int                        (Int8, Int16)
import Data.Map                        (Map)
import Data.Proxy                      (Proxy(Proxy))
import Data.String                     (fromString)
import Data.Text                       (Text)
import Data.Validation                 (Validation(Success, Failure))
import Data.Void                       (Void, absurd)
import System.Random                   (RandomGen)

import qualified Data.ByteString.Char8 as BS.C
import qualified Data.Map              as Map
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Text.Encoding    as Text
import qualified Network.HTTP.Req      as Req
import qualified Text.Megaparsec       as P
import qualified Text.PrettyPrint      as PP

import Test.Tasty.Req.Types
import Test.Tasty.Req.Verify           (Mismatch(..), verify)

import qualified Test.Tasty.Req.Parse.JSON as Json

data WithCommand e
  = WithCommand Command e
    deriving Show

data Error
  = HttpE Req.HttpException
  | NoUrlParse Text
  | UnknownMethod Text
  | NonObjectTopLevel
  | BadReference Ref
  | JsonParseFailure (P.ParseErrorBundle Text Void)
  | VerifyFailed [Mismatch]
  | NonUrlReference (Json.Value VoidF Void)
  | ExpectedResponse
  | UnexpectedResponse
  | forall f a. (ShowF f, Show a) =>
      BadSquash (Json.Value f a) (Json.Value f a)

deriving instance Show Error

type OptionModifier
  = ByteString -> Req.Url 'Req.Http -> Req.Option 'Req.Http

newtype M g e a =
  M (StateT (Map (Int, Side) (Json.Value VoidF Void))
       (EitherT e
          (ReaderT Req.HttpConfig
             (RandT g IO))) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadRandom)

instance Bifunctor (M g) where
  bimap f g (M m) = M $ StateT $ \s ->
    mapEitherT (fmap (bimap f (first g))) $ runStateT m s

instance MonadError e (M g e) where
  throwError e = M (lift (throwError e))
  catchError (M m) h = M $ catchError m $ \err -> case h err of M m' -> m'

instance MonadState (Map (Int, Side) (Json.Value VoidF Void)) (M g e) where
  state = M . state

class AsHttpException e where
  _HttpException :: Prism' e Req.HttpException

instance AsHttpException Req.HttpException where
  _HttpException = id

instance AsHttpException Error where
  _HttpException = prism' HttpE $ \case
    HttpE e -> Just e
    _ -> Nothing

class AsError e where
  _Error :: Prism' e Error

instance AsError Error where
  _Error = id

instance AsHttpException e => Req.MonadHttp (M g e) where
  handleHttpException = M . lift . left . review _HttpException
  getHttpConfig = M (lift $ lift ask)

data FailureMode e
  = Ok
  | Failed (NonEmpty e)
  | Thrown SomeException

sequenceActions
  :: s -> (s -> m () -> IO (Either e s))
  -> [(Bool, m ())]
  -> IO (Either (NonEmpty e) (), s)
sequenceActions s0 f = go s0 Ok
  where
    go s Ok [] =
      pure (Right (), s)
    go s (Failed es) [] =
      pure (Left es, s)
    go _ (Thrown exc) [] =
      throwIO exc
    go s Ok ((_, m):ms) =
      f s m `catch` continue s ms Thrown >>= \case
        Left  e  -> go s  (Failed (e:|[])) ms
        Right s' -> go s' Ok ms
    go s (Failed es) ((True, m):ms) =
      f s m `catch` continue s ms (const $ Failed es) >>= \case
        Left  e  -> go s  (Failed (es <> (e:|[]))) ms
        Right s' -> go s' (Failed es) ms
    go s (Thrown exc) ((True, m):ms) =
      f s m `catch` continue s ms (const $ Thrown exc) >>= \case
        Left  _  -> go s  (Thrown exc) ms
        Right s' -> go s' (Thrown exc) ms
    go s errs ((False, _):ms) =
      go s errs ms
    continue s ms failed exc =
      go s (failed exc) ms >> undefined

runCommands
  :: RandomGen g
  => Text -> OptionModifier -> [Command]
  -> RandT g IO (Either (NonEmpty (WithCommand Error)) ())
runCommands urlPrefix modifier cmds = do
  let httpConfig = def
  let logged =
        [ (command'always c, first (WithCommand c) $ runCommand' urlPrefix modifier c)
        | c <- cmds
        ]
  let evalM (s, g) (M m) =
        runRandT (runReaderT (runEitherT (runStateT m s)) httpConfig) g >>= \case
          (Left e, _)         -> pure (Left e)
          (Right ((), x), g') -> pure (Right (x, g'))
  liftRandT $ \g -> do
    (r, (_, g')) <- sequenceActions (Map.empty, g) evalM logged
    case r of
      Left e -> pure (Left e, g')
      Right () -> pure (Right (), g')

runCommand'
  :: (AsError e, AsHttpException e, RandomGen g)
  => Text -> OptionModifier -> Command -> M g e ()
runCommand' urlPrefix modifier cmd = do
  (url, opts) <- buildUrl urlPrefix (command'url cmd)
  case command'method cmd of
    "GET" ->
      Req.req Req.GET url Req.NoReqBody Req.bsResponse
          (opts <> modifier (Req.httpMethodName (Proxy @Req.GET)) url)
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "POST" -> do
      reqBody <- buildRequestBody (command'id cmd) (command'request_body cmd)
      Req.req Req.POST url reqBody Req.bsResponse
          (opts <> modifier (Req.httpMethodName (Proxy @Req.POST)) url)
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "PUT" -> do
      reqBody <- buildRequestBody (command'id cmd) (command'request_body cmd)
      Req.req Req.PUT url reqBody Req.bsResponse
          (opts <> modifier (Req.httpMethodName (Proxy @Req.PUT)) url)
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "DELETE" ->
      Req.req Req.DELETE url Req.NoReqBody Req.bsResponse
          (opts <> modifier (Req.httpMethodName (Proxy @Req.DELETE)) url)
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    method ->
      throwError (review _Error (UnknownMethod method))

buildUrl
  :: AsError e
  => Text
  -> [Either Ref Text]
  -> M g e (Req.Url 'Req.Http, Req.Option schema)
buildUrl urlPrefix urlParts = do
  let f url [] = pure url
      f url (Right x:xs)  = f (url <> x) xs
      f url (Left ref:xs) = deref ref >>= \case
        Json.JsonNull      -> f (url <> "null") xs
        Json.JsonTrue      -> f (url <> "true") xs
        Json.JsonFalse     -> f (url <> "false") xs
        Json.JsonText    t -> f (url <> t) xs
        Json.JsonInteger n -> f (url <> fromString (show n)) xs
        Json.JsonDouble  n -> f (url <> fromString (show n)) xs
        x                  -> throwError (_Error # NonUrlReference x)
  urlText <- f urlPrefix urlParts
  case Req.parseUrlHttp (Text.encodeUtf8 urlText) of
    Nothing -> throwError (_Error # NoUrlParse urlText)
    Just (url, opts) -> pure (url, opts)

buildRequestBody
  :: (AsError e, RandomGen g)
  => Int
  -> Maybe (Json.Value NonEmpty ReqCustom)
  -> M g e Req.ReqBodyBs
buildRequestBody _ Nothing = pure (Req.ReqBodyBs "")
buildRequestBody i (Just req) = do
  req'
    <-  Json.substitute (resolveRef f) req
    >>= Json.substitute instanceGenerator
    >>= squash
  modify (Map.insert (i, Request) req')
  pure (Req.ReqBodyBs (BS.C.pack (PP.render (Json.ppValue absurd req'))))
  where
    f (ReqRef ref) = Right ref
    f (ReqGen gen) = Left gen

responseParser :: P.ParsecT Void Text Identity (Json.Value VoidF Void)
responseParser = Json.runParser Json.combineVoidF (Json.object <> Json.array) <* P.eof

parseResponse :: AsError e => Req.BsResponse -> M g e (Maybe (Json.Value VoidF Void))
parseResponse resp
  | BS.C.null (Req.responseBody resp) = pure Nothing
  | otherwise = either badParse (pure . Just) $ P.parse responseParser "" respText
  where
    respText = Text.decodeUtf8 $ Req.responseBody resp
    badParse = throwError . review _Error . JsonParseFailure

verifyResponse
  :: AsError e
  => Int
  -> Maybe (Json.Value NonEmpty RespCustom)
  -> Maybe (Json.Value VoidF Void)
  -> M g e ()
verifyResponse i p_val val = case (p_val, val) of
  (Nothing, Nothing) -> pure ()
  (Just p_x, Just x) -> do
    p_x' <- Json.substitute (resolveRef f) p_x >>= squash
    case verify p_x' x of
      Failure errs -> throwError (_Error # VerifyFailed errs)
      Success y    -> modify (Map.insert (i, Response) y)
  (Just _, Nothing) -> throwError (_Error # ExpectedResponse)
  (Nothing, Just _) -> throwError (_Error # UnexpectedResponse)
  where
    f (RespRef ref)     = Right ref
    f (RespTypeDefn ty) = Left (Right ty)

resolveRef :: AsError e => (a -> Either b Ref) -> Text -> a -> M g e (Json.Value NonEmpty b)
resolveRef f nm x = case f x of
  Left y    -> pure (Json.JsonCustom nm y)
  Right ref -> Json.injectVoidF . fmap absurd <$> deref ref

deref :: AsError e => Ref -> M g e (Json.Value VoidF Void)
deref ref@(Ref i side path0 sans) = do
  m <- gets (Map.lookup (i, side))
  case m of
    Nothing -> throwError (_Error # BadReference ref)
    Just x -> do
      x' <- go path0 x
      case (Set.null sans, x') of
        (True, _) -> pure x'
        (False, Json.JsonObject (Json.Object o)) -> do
          unless (sans `Set.isSubsetOf` Map.keysSet o) $
            throwError undefined
          pure (Json.JsonObject (Json.Object (Map.withoutKeys o sans)))
        (False, _) -> throwError undefined
  where
    go [] o = pure o
    go (Left j:path) (Json.JsonArray xs) =
      case drop j xs of
        o':_    -> go path o'
        []      -> throwError (_Error # BadReference ref)
    go (Right k:path) (Json.JsonObject (Json.Object o)) =
      case Map.lookup k o of
        Just o' -> go path o'
        Nothing -> throwError (_Error # BadReference ref)
    go _ _ = throwError (_Error # BadReference ref)

instanceGenerator :: RandomGen g => Text -> Generator -> M g e (Json.Value f Void)
instanceGenerator nm = \case
  GenString    -> Json.JsonText . Text.pack . ("foobar"++) . show <$> getRandom @_ @Int16
  GenInteger   -> Json.JsonInteger <$> getRandom
  GenDouble    -> Json.JsonDouble <$> getRandom
  GenBool      -> (\b -> if b then Json.JsonTrue else Json.JsonFalse) <$> getRandom
  GenMaybe gen -> do
    b <- getRandom @_ @Int8
    if b < 16 then pure Json.JsonNull else instanceGenerator nm gen

squash
  :: (AsError e, Traversable f, Show a)
  => Json.Value f a -> M g e (Json.Value VoidF a)
squash = Json.elimCombine f
  where
    f (Json.JsonObject a) (Json.JsonObject b) = pure (Json.JsonObject (a <> b))
    f x y = throwError (_Error # BadSquash x y)
