{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE LambdaCase         #-}

module Test.Tasty.Req.Types
  ( Command(..)
  , Ref(..), Side(..), TypeDefn(..)
    -- * JSON
  , Value(..), Object(..)
    -- * Higher-kinded Show class
  , ShowF(..)
    -- * Higher-kinded empty type
  , VoidF, absurdF
  ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map           (Map)
import Data.Text          (Text)

class ShowF f where
  showF :: Show a => f a -> String
  showsPrecF :: Show a => Int -> f a -> ShowS

instance ShowF NonEmpty where
  showF = show
  showsPrecF = showsPrec

newtype WrapShowF f a = WrapShowF (f a)

instance (ShowF f, Show a) => Show (WrapShowF f a) where
  showsPrec p (WrapShowF fa) = showsPrecF p fa

showsApp1 :: Show a => String -> Int -> a -> ShowS
showsApp1 s p a = strApp1 s p (showsPrec 11 a)

showsFApp1 :: (ShowF f, Show a) => String -> Int -> f a -> ShowS
showsFApp1 s p fa = strApp1 s p (showsPrecF 11 fa)

strApp1 :: String -> Int -> ShowS -> ShowS
strApp1 s p sh = showParen (p >= 11) $ showString s . showChar ' ' . sh

data Ref = Ref Int Side [Either Int Text]
  deriving (Eq, Ord, Show)

data Side
  = Request
  | Response
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

data Command = Command
  { command'id            :: Int
  , command'always        :: Bool
  , command'method        :: Text
  , command'url           :: [Either Ref Text]
  , command'request_body  :: Maybe (Value NonEmpty Ref)
  , command'response_body :: Maybe (Value NonEmpty (Either Ref TypeDefn))
  } deriving Show

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
    JsonCustom nm x -> showParen (d > app_prec) $
      showString "JsonCustom "
      . shows nm
      . showChar ' '
      . showsPrec (app_prec+1) x
    where
      app_prec = 10

newtype Object f a
  = Object (Map Text (Value f a))
    deriving (Functor, Foldable, Traversable)

instance (ShowF f, Show a) => Show (Object f a) where
  showsPrec d (Object o) = showsApp1 "Object" d o

instance Semigroup (Object f a) where
  Object a <> Object b = Object (a <> b)

instance Monoid (Object f a) where
  mempty = Object mempty
  mappend = (<>)

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

instance ShowF VoidF where
  showF = absurdF
  showsPrecF _ = absurdF
