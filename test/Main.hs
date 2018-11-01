module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Tasty.Req.Parse.Test as Parse

main :: IO ()
main = do
  scriptParseTests <- Parse.mkScriptTests "test-golden/script-parser"
  defaultMain $ testGroup "ALL"
    [ scriptParseTests
    ]
