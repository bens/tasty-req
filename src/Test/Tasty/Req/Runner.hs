{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

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
import Data.Foldable                   (fold)
import Data.Functor.Identity           (Identity)
import Data.Map                        (Map)
import Data.String                     (fromString)
import Data.Text                       (Text)
import Data.Void                       (Void, absurd)
import Data.Validation                 (Validation(Success, Failure))

import qualified Data.ByteString.Char8 as BS.C
import qualified Data.Map              as Map
import qualified Data.Text.Encoding    as Text
import qualified Network.HTTP.Req      as Req
import qualified Text.Megaparsec       as P
import qualified Text.PrettyPrint      as PP

import Test.Tasty.Req.Types            (Command(..), Ref(..), Side(..), TypeDefn(..))
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
  | NonUrlReference (Json.Value Void)
    deriving (Show)

newtype M a =
  M (StateT (Map (Int, Side) (Json.Value Void))
       (EitherT Error
          (ReaderT Req.HttpConfig IO)) a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadError Error M where
  throwError e = M (lift (throwError e))
  catchError (M m) h = M $ catchError m $ \err -> case h err of M m' -> m'

instance MonadState (Map (Int, Side) (Json.Value Void)) M where
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
    "GET" -> do
      resp <- Req.req Req.GET url Req.NoReqBody Req.bsResponse opts >>= parseResponse cmd
      case resp of
        Just r -> verifyResponse (command'id cmd) (command'response_body cmd) r
        Nothing -> pure ()
    "POST" -> do
      reqObj <- buildRequestBody (command'id cmd) (command'request_body cmd)
      let reqBody = Req.ReqBodyBs (BS.C.pack (PP.render (Json.ppValue absurd reqObj)))
      resp <- Req.req Req.POST url reqBody Req.bsResponse opts >>= parseResponse cmd
      case resp of
        Just r -> verifyResponse (command'id cmd) (command'response_body cmd) r
        Nothing -> pure ()
    "PUT" -> do
      reqObj <- buildRequestBody (command'id cmd) (command'request_body cmd)
      let reqBody = Req.ReqBodyBs (BS.C.pack (PP.render (Json.ppValue absurd reqObj)))
      resp <- Req.req Req.PUT url reqBody Req.bsResponse opts >>= parseResponse cmd
      case resp of
        Just r -> verifyResponse (command'id cmd) (command'response_body cmd) r
        Nothing -> pure ()
    "DELETE" -> do
      resp <- Req.req Req.DELETE url Req.NoReqBody Req.bsResponse opts >>= parseResponse cmd
      case resp of
        Just r -> verifyResponse (command'id cmd) (command'response_body cmd) r
        Nothing -> pure ()
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

buildRequestBody :: Int -> [Json.Value Ref] -> M (Json.Value Void)
buildRequestBody i os = do
  o <- fmap Json.JsonObject (resolveRefs Right os)
  o <$ modify (Map.insert (i, Request) o)

parseResponse :: Command -> Req.BsResponse -> M (Maybe (Json.Value Void))
parseResponse cmd resp = case command'response_body cmd of
  [] | BS.C.null (Req.responseBody resp) -> pure Nothing
     | otherwise -> throwError undefined
  _ -> either (throwError . JsonParseFailure) (pure . Just) $ P.parse parser "" respText
  where
    parser   = Json.runParser Json.value :: P.ParsecT Void Text Identity (Json.Value Void)
    respText = Text.decodeUtf8 $ Req.responseBody resp

verifyResponse :: Int -> [Json.Value (Either Ref TypeDefn)] -> Json.Value Void -> M ()
verifyResponse i expected x = do
  resolved <- resolveRefs (either Right Left) expected
  case verify (Json.JsonObject resolved) x of
    Failure errs -> throwError (VerifyFailed errs)
    Success y    -> modify (Map.insert (i, Response) y)

resolveRefs :: (a -> Either b Ref) -> [Json.Value a] -> M (Json.Object b)
resolveRefs f xs = fold <$> (traverse (Json.substitute resolve) xs >>= mapM extract)
  where
    resolve nm x = case f x of
      Left y    -> pure (Json.JsonCustom nm y)
      Right ref -> fmap absurd <$> deref ref
    extract = \case
      Json.JsonObject o -> pure o
      _                 -> throwError NonObjectTopLevel

deref :: Ref -> M (Json.Value Void)
deref ref@(Ref i side path0) = do
  m <- gets (Map.lookup (i, side))
  case m of
    Nothing -> throwError (BadReference ref)
    Just x -> go path0 x
      where
        go [] o = pure o
        go (path:paths) (Json.JsonObject (Json.Object o)) = case Map.lookup path o of
          Just o' -> go paths o'
          Nothing -> throwError undefined
        go (_path:_paths) _ = throwError undefined
