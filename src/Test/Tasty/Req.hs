{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Req
  ( reqTest
  ) where

import Control.Exception     (SomeException, try)
import Control.Monad.Random  (evalRandT)
import Data.Foldable         (toList)
import Data.Functor.Identity (Identity)
import Data.Text             (Text)
import Data.Void             (Void)
import System.Random         (newStdGen)
import Text.PrettyPrint      (nest, render)
import Text.Show.Pretty      (ppDoc)

import qualified Data.Text.IO         as Text
import qualified Network.HTTP.Req     as Req
import qualified Test.Tasty.Options   as Tasty
import qualified Test.Tasty.Providers as Tasty
import qualified Text.Megaparsec      as P
import qualified Text.PrettyPrint     as PP

import Test.Tasty.Req.Parse  (pScript)
import Test.Tasty.Req.Runner (Error(..), OptionModifier, WithCommand(..), runCommands)
import Test.Tasty.Req.Types  (Command)

reqTest :: Tasty.TestName -> FilePath -> IO Text -> OptionModifier -> Tasty.TestTree
reqTest testName scriptPath getUrlPrefix modifier =
  Tasty.singleTest testName $ ReqTest scriptPath getUrlPrefix modifier

data ReqTest = ReqTest FilePath (IO Text) OptionModifier

instance Tasty.IsTest ReqTest where
  run options (ReqTest scriptPath getUrlPrefix modifier) _progress = do
    urlPrefix <- getUrlPrefix
    runIt options scriptPath urlPrefix modifier
  testOptions = pure []

runIt :: Tasty.OptionSet -> FilePath -> Text -> OptionModifier -> IO Tasty.Result
runIt _options scriptPath urlPrefix modifier = do
  script <- Text.readFile scriptPath
  let p = pScript :: P.ParsecT Void Text Identity [Command]
  try (pure $! P.parse p scriptPath script) >>= \case
    Left exc ->
      pure (Tasty.testFailed (render $ ppDoc (exc :: SomeException)))
    Right (Left err) ->
      pure (Tasty.testFailed (P.errorBundlePretty err))
    Right (Right cmds) -> do
      g <- newStdGen
      evalRandT (runCommands urlPrefix modifier cmds) g >>= \case
        Left errs ->
          pure $ Tasty.testFailed $ render $ PP.vcat $ map PP.vcat
            [ [ ppDoc cmd
              , PP.text "Error:"
              , nest 4 $ case err of
                  HttpE (Req.VanillaHttpException exc) -> ppDoc exc
                  _                                    -> ppDoc err
              ]
            | WithCommand cmd err <- toList errs
            ]
        Right () ->
          pure $ Tasty.testPassed ""
