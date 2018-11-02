{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Req
  ( reqTest
  ) where

import Control.Monad.Random           (evalRandT)
import Control.Exception              (SomeException, try)
import Data.Functor.Identity          (Identity)
import Data.Text                      (Text)
import Data.Void                      (Void)
import System.Random                  (newStdGen)
import Text.Show.Pretty               (ppShow)

import qualified Data.Text.IO         as Text
import qualified Network.HTTP.Req     as Req
import qualified Test.Tasty.Options   as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Text.Megaparsec      as P

import Test.Tasty.Req.Parse           (parser)
import Test.Tasty.Req.Runner          (Error(..), WithCommand(..), runCommands)
import Test.Tasty.Req.Types           (Command)

reqTest :: Tasty.TestName -> FilePath -> Text -> Tasty.TestTree
reqTest testName scriptPath urlPrefix =
  Tasty.singleTest testName $ ReqTest scriptPath urlPrefix

data ReqTest = ReqTest FilePath Text

instance Tasty.IsTest ReqTest where
  run options (ReqTest scriptPath urlPrefix) _progress =
    runIt options scriptPath urlPrefix
  testOptions = pure []

runIt :: Tasty.OptionSet -> FilePath -> Text -> IO Tasty.Result
runIt _options scriptPath urlPrefix = do
  script <- Text.readFile scriptPath
  let p = parser :: P.ParsecT Void Text Identity [Command]
  try (pure $! P.parse p scriptPath script) >>= \case
    Left exc ->
      pure (Tasty.testFailed (ppShow (exc :: SomeException)))
    Right (Left err) ->
      pure (Tasty.testFailed (P.errorBundlePretty err))
    Right (Right cmds) -> do
      g <- newStdGen
      evalRandT (runCommands urlPrefix cmds) g >>= \case
        Left (WithCommand cmd (HttpE (Req.VanillaHttpException exc))) ->
          pure $ Tasty.testFailed (ppShow (cmd, exc))
        Left (WithCommand cmd err) ->
          pure $ Tasty.testFailed (ppShow (cmd, err))
        Right () ->
          pure $ Tasty.testPassed ""
