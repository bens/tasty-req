{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Req.Parse.Test
  ( mkScriptTests
  ) where

import Control.Exception         (SomeException, try)
import Data.Functor.Identity     (Identity)
import Control.Monad             (forM)
import Data.List                 (sort)
import Data.Text                 (Text)
import Data.Void                 (Void)
import System.FilePath           (replaceExtension)
import Test.Tasty                (TestTree, testGroup)
import Test.Tasty.Golden         (findByExtension, goldenVsFileDiff)
import Text.Show.Pretty          (ppShow)

import qualified Data.Text.IO    as Text
import qualified Text.Megaparsec as P

import Test.Tasty.Req.Parse      (parser)
import Test.Tasty.Req.Types      (Command)

mkScriptTests :: FilePath -> IO TestTree
mkScriptTests dir = do
  reqPaths <- sort <$> findByExtension [".req"] dir
  ts <- forM reqPaths $ \reqPath -> do
    let goldenPath = replaceExtension reqPath ".golden"
        outPath    = replaceExtension reqPath ".out"
    return $ goldenVsFileDiff reqPath diff goldenPath outPath (go reqPath outPath)
  pure $ testGroup dir ts
  where
    diff ref new = ["diff", "-u", ref, new]
    go reqPath outPath = do
      script <- Text.readFile reqPath
      let p = parser :: P.ParsecT Void Text Identity [Command]
      try (pure $! P.parse p reqPath script) >>= \case
        Left exc -> let _ = exc :: SomeException in writeFile outPath (show exc ++ "\n")
        Right (Left err) -> writeFile outPath (P.errorBundlePretty err)
        Right (Right x) -> writeFile outPath (ppShow x ++ "\n")
