{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Tasty.Req.Runner
  ( Error(..), runCommands
  ) where

import Control.Monad.Except            (MonadError(..))
import Control.Monad.IO.Class          (MonadIO)
import Control.Monad.State             (MonadState(..), gets, modify)
import Control.Monad.Trans.Class       (MonadTrans(lift))
import Control.Monad.Trans.Either      (EitherT, left, runEitherT)
import Control.Monad.Trans.Reader      (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State       (StateT, evalStateT)
import Data.Default                    (def)
import Data.Functor.Identity           (Identity)
import Data.List.NonEmpty              (NonEmpty)
import Data.Map                        (Map)
import Data.String                     (fromString)
import Data.Text                       (Text)
import Data.Validation                 (Validation(Success, Failure))
import Data.Void                       (Void, absurd)

import qualified Data.ByteString.Char8 as BS.C
import qualified Data.Map              as Map
import qualified Data.Text.Encoding    as Text
import qualified Network.HTTP.Req      as Req
import qualified Text.Megaparsec       as P
import qualified Text.PrettyPrint      as PP

import Test.Tasty.Req.Types            (Command(..), Ref(..), Side(..), TypeDefn(..), VoidF)
import Test.Tasty.Req.Verify           (Mismatch(..), verify)

import qualified Test.Tasty.Req.Parse.JSON as Json

data Error
  = HttpE Req.HttpException
  | NoUrlParse Text
  | UnknownMethod Text
  | NonObjectTopLevel
  | BadReference Ref
  | JsonParseFailure (P.ParseErrorBundle Text Void)
  | VerifyFailed [Mismatch]
  | NonUrlReference (Json.Value VoidF Void)
    deriving Show

newtype M a =
  M (StateT (Map (Int, Side) (Json.Value VoidF Void))
       (EitherT Error
          (ReaderT Req.HttpConfig IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadError Error M where
  throwError e = M (lift (throwError e))
  catchError (M m) h = M $ catchError m $ \err -> case h err of M m' -> m'

instance MonadState (Map (Int, Side) (Json.Value VoidF Void)) M where
  state = M . state

instance Req.MonadHttp M where
  handleHttpException = M . lift . left . HttpE
  getHttpConfig = M (lift $ lift ask)

runCommands :: Text -> [Command] -> IO (Either Error ())
runCommands urlPrefix cmds = do
  let httpConfig = def
  case mapM_ (runCommand' urlPrefix) cmds of
    M m -> runReaderT (runEitherT (evalStateT m Map.empty)) httpConfig

runCommand' :: Text -> Command -> M ()
runCommand' urlPrefix cmd = do
  (url, opts) <- buildUrl urlPrefix (command'url cmd)
  case command'method cmd of
    "GET" ->
      Req.req Req.GET url Req.NoReqBody Req.bsResponse opts
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "POST" -> do
      reqBody <- buildRequestBody (command'id cmd) (command'request_body cmd)
      Req.req Req.POST url reqBody Req.bsResponse opts
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "PUT" -> do
      reqBody <- buildRequestBody (command'id cmd) (command'request_body cmd)
      Req.req Req.PUT url reqBody Req.bsResponse opts
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    "DELETE" ->
      Req.req Req.DELETE url Req.NoReqBody Req.bsResponse opts
        >>= parseResponse
        >>= verifyResponse (command'id cmd) (command'response_body cmd)
    method ->
      throwError (UnknownMethod method)

buildUrl :: Text -> [Either Ref Text] -> M (Req.Url 'Req.Http, Req.Option schema)
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
        x                  -> throwError (NonUrlReference x)
  urlText <- f urlPrefix urlParts
  case Req.parseUrlHttp (Text.encodeUtf8 urlText) of
    Nothing -> throwError (NoUrlParse urlText)
    Just (url, opts) -> pure (url, opts)

buildRequestBody :: Int -> Maybe (Json.Value NonEmpty Ref) -> M Req.ReqBodyBs
buildRequestBody _ Nothing = pure (Req.ReqBodyBs "")
buildRequestBody i (Just os) = do
  o <- resolveRefs Right os >>= squash
  modify (Map.insert (i, Request) o)
  pure (Req.ReqBodyBs (BS.C.pack (PP.render (Json.ppValue absurd o))))

parseResponse :: Req.BsResponse -> M (Maybe (Json.Value VoidF Void))
parseResponse resp
  | BS.C.null (Req.responseBody resp) = pure Nothing
  | otherwise = either badParse (pure . Just) $ P.parse parser "" respText
  where
    parser   = Json.runParser Json.combineVoidF Json.value :: P.ParsecT Void Text Identity (Json.Value VoidF Void)
    respText = Text.decodeUtf8 $ Req.responseBody resp
    badParse = throwError . JsonParseFailure

verifyResponse
  :: Int
  -> Maybe (Json.Value NonEmpty (Either Ref TypeDefn))
  -> Maybe (Json.Value VoidF Void)
  -> M ()
verifyResponse _ Nothing Nothing = pure ()
verifyResponse i (Just expected) (Just x) = do
  resolved <- resolveRefs (either Right Left) expected >>= squash
  case verify resolved x of
    Failure errs -> throwError (VerifyFailed errs)
    Success y    -> modify (Map.insert (i, Response) y)
verifyResponse _ (Just _) Nothing = throwError undefined
verifyResponse _ Nothing (Just _) = throwError undefined

resolveRefs :: (a -> Either b Ref) -> Json.Value NonEmpty a -> M (Json.Value NonEmpty b)
resolveRefs f = Json.substitute resolve
  where
    resolve nm x = case f x of
      Left y    -> pure (Json.JsonCustom nm y)
      Right ref -> Json.injectVoidF . fmap absurd <$> deref ref

deref :: Ref -> M (Json.Value VoidF Void)
deref ref@(Ref i side path0) = do
  m <- gets (Map.lookup (i, side))
  case m of
    Nothing -> throwError (BadReference ref)
    Just x -> go path0 x
      where
        go [] o = pure o
        go (Left j:path) (Json.JsonArray xs) = case drop j xs of
          o':_ -> go path o'
          [] -> throwError undefined
        go (Right k:path) (Json.JsonObject (Json.Object o)) = case Map.lookup k o of
          Just o' -> go path o'
          Nothing -> throwError undefined
        go (_path:_paths) _ = throwError undefined

squash :: Traversable f => Json.Value f a -> M (Json.Value VoidF a)
squash = Json.elimCombine f
  where
    f (Json.JsonObject a) (Json.JsonObject b)= pure (Json.JsonObject (a <> b))
    f _ _ = throwError undefined
