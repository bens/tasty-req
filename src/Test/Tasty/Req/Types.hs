{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Tasty.Req.Types
  ( ReqCustom(..), RespCustom(..), Command(..)
  , Ref(..), Side(..), Generator(..), TypeDefn(..)
    -- * JSON
  , Json, JsonF(..)
    -- * JSON with custom values
  , CustomF(..), embedJsonF', elimCustom, elimCustomAlg
    -- * JSON with custom values and merging
  , MergeF(..), mergeEmbedJson
  , patternCustoms, patternCustomsFinal
  , elimMerge, elimMergeAlg
  , PatternF, Pattern
  ) where

import Control.Lens         (ATraversal, Traversal, cloneTraversal, mapMOf)
import Control.Monad        (foldM, (>=>))
import Control.Recursion    (Base, Fix(Fix), Recursive(..), cata, cataM)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Data.List.NonEmpty   (NonEmpty((:|)))
import Data.Map             (Map)
import Data.Set             (Set)
import Data.Text            (Text)
import Data.Void            (Void, absurd)

strApp1 :: String -> Int -> ShowS -> ShowS
strApp1 s p sh = showParen (p >= 11) $ showString s . showChar ' ' . sh

-- Command AST

data Ref = Ref Int Side [Either Int Text] (Set Text)
  deriving (Eq, Ord, Show)

data Side
  = Request
  | Response
    deriving (Eq, Ord, Show)

data Generator
  = GenText
  | GenInt
  | GenDouble
  | GenBool
  | GenMaybe Generator
    deriving (Eq, Ord, Show)

data TypeDefn
  = TyAny
  | TyText
  | TyInt
  | TyDouble
  | TyBool
  | TyObject
  | TyArray
  | TyMaybe TypeDefn
    deriving (Eq, Ord, Show)

data ReqCustom
  = ReqRef Ref
  | ReqGen Generator
    deriving (Eq, Show)

data RespCustom
  = RespRef Ref
  | RespTypeDefn TypeDefn
    deriving (Eq, Show)
data Command = Command
  { command'id            :: Int
  , command'always        :: Bool
  , command'method        :: Text
  , command'url           :: [Either Ref Text]
  , command'request_body  :: Maybe (Pattern ReqCustom)
  , command'response_body :: Maybe (Pattern RespCustom)
  } deriving (Eq, Show)


-- JSON values

type Json = Fix JsonF

data JsonF a
  = NullF
  | TrueF
  | FalseF
  | TextF Text
  | IntF Int
  | DoubleF Double
  | ObjectF (Map Text a)
  | ArrayF [a]
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance Eq1 JsonF where
  liftEq op a b = case (a, b) of
    (  NullF  ,   NullF  ) -> True
    (  TrueF  ,   TrueF  ) -> True
    ( FalseF  ,  FalseF  ) -> True
    (  TextF x,   TextF y) -> x == y
    (   IntF x,    IntF y) -> x == y
    (DoubleF x, DoubleF y) -> x == y
    (ObjectF x, ObjectF y) -> liftEq op x y
    ( ArrayF x,  ArrayF y) -> liftEq op x y
    (        _,         _) -> False

instance Show1 JsonF where
  liftShowsPrec p p_list d = \case
    NullF     -> showString "NullF"
    TrueF     -> showString "TrueF"
    FalseF    -> showString "FalseF"
    TextF   x -> strApp1 "TextF"   d (showsPrec d x)
    IntF    x -> strApp1 "IntF"    d (showsPrec d x)
    DoubleF x -> strApp1 "DoubleF" d (showsPrec d x)
    ObjectF o -> strApp1 "ObjectF" d (liftShowsPrec p p_list d o)
    ArrayF xs -> strApp1 "ArrayF"  d (liftShowsPrec p p_list d xs)


-- JSON patterns without merging

data CustomF x a
  = C (Text, x)
  | J (JsonF a)
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

instance Eq x => Eq1 (CustomF x) where
  liftEq  _ (C x) (C y) = x == y
  liftEq op (J x) (J y) = liftEq op x y
  liftEq _ _ _          = False

instance Show x => Show1 (CustomF x) where
  liftShowsPrec p p_list d = \case
    C x -> strApp1 "C" d (showsPrec 11 x)
    J x -> strApp1 "J" d (liftShowsPrec p p_list 11 x)

patternCustoms
  :: Traversal
       (PatternF a (Pattern b))
       (PatternF b (Pattern b))
       (Text, a)
       (Either b (JsonF (Pattern b)))
patternCustoms f (M x xs) = M <$> g x <*> traverse g xs
  where
    g (C (nm, a)) = either (C . (nm,)) J <$> f (nm, a)
    g (J j)       = pure (J j)

patternCustomsFinal
  :: Traversal
       (PatternF a (Fix (MergeF JsonF)))
       (MergeF JsonF (Fix (MergeF JsonF)))
       (Text, a)
       (Either Void (JsonF (Fix (MergeF JsonF))))
patternCustomsFinal f (M x xs) = M <$> g x  <*> traverse g xs
  where
    g (C (nm, a)) = either absurd id <$> f (nm, a)
    g (J j)       = pure j

elimCustom
  :: (Traversable f, Monad m)
  => ATraversal (f (Fix g)) (g (Fix g)) (Text, a) (Either b (JsonF (Fix g)))
  -> (Text -> a -> m (Either b (JsonF (Fix g))))
  -> Fix f
  -> m (Fix g)
elimCustom trav subst = cataM (elimCustomAlg trav subst)

elimCustomAlg
  :: Monad m
  => ATraversal (f (Fix g)) (g (Fix g)) (Text, a) (Either b (JsonF (Fix g)))
  -> (Text -> a -> m (Either b (JsonF (Fix g))))
  -> f (Fix g)
  -> m (Fix g)
elimCustomAlg trav subst =
  fmap Fix . mapMOf (cloneTraversal trav) (uncurry subst)

embedJsonF' :: Json -> Fix (CustomF x)
embedJsonF' = cata $ Fix . J


-- JSON patterns with merging

type PatternF a = MergeF (CustomF a)

type Pattern a = Fix (PatternF a)

data MergeF f a = M (f a) [f a]
  deriving (Functor, Foldable, Traversable, Eq, Show)

instance Eq1 f => Eq1 (MergeF f) where
  liftEq op a b = case (a, b) of
    (M x xs, M y ys) ->
      liftEq op x y && liftEq (liftEq op) xs ys

instance Show1 f => Show1 (MergeF f) where
  liftShowsPrec p p_list d (M x xs) =
    strApp1 "M" d
      $ liftShowsPrec p p_list 11 x
      . showChar ' '
      . liftShowsPrec (liftShowsPrec p p_list) (liftShowList p p_list) 11 xs

mergeEmbedJson :: Json -> Pattern a
mergeEmbedJson = cata $ Fix . flip M [] . J

elimMerge
  :: (Traversable f, Monad m)
  => (MergeF f (Fix f) -> m (NonEmpty (f (Fix f))))
  -> (f (Fix f) -> f (Fix f) -> m (f (Fix f)))
  -> Fix (MergeF f)
  -> m (Fix f)
elimMerge ex op = cataM (elimMergeAlg ex op)

elimMergeAlg
  :: Monad m
  => (MergeF a (Fix f) -> m (NonEmpty (f (Fix f))))
  -> (f (Fix f) -> f (Fix f) -> m (f (Fix f)))
  -> MergeF a (Fix f)
  -> m (Fix f)
elimMergeAlg ex op =
  ex >=> \(y :| ys) -> Fix <$> foldM op y ys


-- Orphans

instance Eq1 f => Eq (Fix f) where
  Fix x == Fix y = liftEq (==) x y

instance Show1 f => Show (Fix f) where
  showsPrec d (Fix x) = showParen (d > 10) $ liftShowsPrec showsPrec showList 11 x
