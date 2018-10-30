module Test.Tasty.Req.Types
  ( Command(..)
  , Ref(..), Side(..), TypeDefn(..)
  , Json.Value(..)
  ) where

import Data.Text                            (Text)

import qualified Test.Tasty.Req.Parse.JSON as Json

data Ref = Ref Int Side [Text]
  deriving (Eq, Ord, Show)

data Side
  = Request
  | Response
    deriving (Eq, Ord, Show)

data TypeDefn
  = TyString
  | TyInteger
  | TyDouble
  | TyBool
  | TyMaybe TypeDefn
    deriving (Eq, Ord, Show)

data Command = Command
  { command'id            :: Int
  , command'always        :: Bool
  , command'method        :: Text
  , command'url           :: [Either Ref Text]
  , command'request_body  :: [Json.Value Ref]
  , command'response_body :: [Json.Value (Either Ref TypeDefn)]
  } deriving (Eq, Ord, Show)
