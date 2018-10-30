module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Tasty.Req.Parse.Test as Parse

main :: IO ()
main = do
  parseTests <- Parse.mkTests "test-golden/parser"
  defaultMain $ testGroup "ALL"
    [ parseTests
    ]
