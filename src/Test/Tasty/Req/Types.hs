{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Test.Tasty.Req.Types
  ( ReqCustom(..), RespCustom(..), Command(..)
  , Ref(..), Side(..), Generator(..), TypeDefn(..)
    -- * JSON
  , Value(..), Object(..)
    -- * Eq and Show for Functors
  , EqF(..), ShowF(..)
    -- * Functor empty type
  , VoidF, absurdF
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map           (Map)
import Data.Set           (Set)
import Data.Text          (Text)

-- EqF class

class EqF f where
  eqF :: Eq a => f a -> f a -> Bool

instance EqF NonEmpty where eqF = (==)
instance EqF VoidF    where eqF = absurdF

-- ShowF class

class ShowF f where
  showF :: Show a => f a -> String
  showsPrecF :: Show a => Int -> f a -> ShowS

instance ShowF NonEmpty where showF = show;    showsPrecF   = showsPrec
instance ShowF VoidF    where showF = absurdF; showsPrecF _ = absurdF

strApp1 :: String -> Int -> ShowS -> ShowS
strApp1 s p sh = showParen (p >= 11) $ showString s . showChar ' ' . sh

showsApp1 :: Show a => String -> Int -> a -> ShowS
showsApp1 s p a = strApp1 s p (showsPrec 11 a)

showsFApp1 :: (ShowF f, Show a) => String -> Int -> f a -> ShowS
showsFApp1 s p fa = strApp1 s p (showsPrecF 11 fa)

-- Command AST

data Ref = Ref Int Side [Either Int Text] (Set Text)
  deriving (Eq, Ord, Show)

data Side
  = Request
  | Response
    deriving (Eq, Ord, Show)

data Generator
  = GenString
  | GenInteger
  | GenDouble
  | GenBool
  | GenMaybe Generator
    deriving (Eq, Ord, Show)

data TypeDefn
  = TyAny
  | TyString
  | TyInteger
  | TyDouble
  | TyBool
  | TyObject
  | TyArray
  | TyMaybe TypeDefn
    deriving (Eq, Ord, Show)

data ReqCustom
  = ReqRef Ref
  | ReqGen Generator
    deriving Show

data RespCustom
  = RespRef Ref
  | RespTypeDefn TypeDefn
    deriving Show

data Command = Command
  { command'id            :: Int
  , command'always        :: Bool
  , command'method        :: Text
  , command'url           :: [Either Ref Text]
  , command'request_body  :: Maybe (Value NonEmpty ReqCustom)
  , command'response_body :: Maybe (Value NonEmpty RespCustom)
  } deriving Show

-- JSON Values

data Value f a
  = JsonNull
  | JsonTrue
  | JsonFalse
  | JsonText Text
  | JsonInteger Int
  | JsonDouble Double
  | JsonObject (Object f a)
  | JsonArray [Value f a]
  | JsonCustom Text a
  | JsonCombine (f (Value f a))
    deriving (Functor, Foldable, Traversable)

instance (EqF f, Eq a) => Eq (Value f a) where
  JsonNull        == JsonNull         = True
  JsonTrue        == JsonTrue         = True
  JsonFalse       == JsonFalse        = True
  JsonText x      == JsonText y       = x == y
  JsonInteger x   == JsonInteger y    = x == y
  JsonDouble x    == JsonDouble y     = x == y
  JsonObject x    == JsonObject y     = x == y
  JsonArray xs    == JsonArray ys     = xs == ys
  JsonCombine xs  == JsonCombine ys   = eqF xs ys
  JsonCustom nm x == JsonCustom nm' y = nm == nm' && x == y
  _               == _                = False

-- instance (OrdF f, Ord a) => Ord (Value f a) where
--   compare JsonNull

instance (ShowF f, Show a) => Show (Value f a) where
  showsPrec d = \case
    JsonNull        -> showString "JsonNull"
    JsonTrue        -> showString "JsonTrue"
    JsonFalse       -> showString "JsonFalse"
    JsonText x      -> showsApp1  "JsonText"    d x
    JsonInteger n   -> showsApp1  "JsonInteger" d n
    JsonDouble n    -> showsApp1  "JsonDouble"  d n
    JsonObject o    -> showsApp1  "JsonObject"  d o
    JsonArray xs    -> showsApp1  "JsonArray"   d xs
    JsonCombine xs  -> showsFApp1 "JsonCombine" d xs
    JsonCustom nm x -> showsApp1  ("JsonCustom " ++ show nm) d x

-- JSON Objects

newtype Object f a
  = Object (Map Text (Value f a))
    deriving (Eq, Show, Functor, Foldable, Monoid, Semigroup, Traversable)

-- Empty functor type

data VoidF a

absurdF :: VoidF a -> b
absurdF x = case x of {}

instance Functor VoidF where
  fmap _ = absurdF

instance Foldable VoidF where
  foldMap _ = absurdF

instance Traversable VoidF where
  traverse _ = absurdF

instance Semigroup (VoidF a) where
  x <> _ = absurdF x
