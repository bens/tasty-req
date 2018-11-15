{-# LANGUAGE TupleSections #-}

module Test.Tasty.Req.Verify
  ( Mismatch(..), verify
  ) where

import Control.Monad     (unless)
import Control.Recursion (Fix(..))
import Data.AEq          ((~==))
import Data.Map          (Map)
import Data.String       (fromString)
import Data.Text         (Text)
import Data.Traversable  (for)
import Data.Validation   (Validation(Failure))

import qualified Data.Map as Map
import qualified Data.Set as Set

import Test.Tasty.Req.Types

data Mismatch
  = WrongValue          [Text] (Fix (CustomF TypeDefn)) Json
  | WrongType           [Text] TypeDefn Json
  | UnexpectedKeys      [Text] [Text]
  | MissingKeys         [Text] [Text]
  | ArrayLengthMismatch [Text] Int Int
    deriving (Eq, Show)

-- TODO: it would be nice to pass a list of continuations to each of the custom
-- handlers where they could choose to invoke any of them and so override
-- verification of their ancestor elements.  Also, they should be able to make
-- use of their siblings' verification actions in overriding their ancestors.
-- Using this, the array-many functionality can use its sibling's verification
-- many times, to verify the entire array that it sits within.

verify
  :: Fix (CustomF TypeDefn)
  -> Json
  -> Validation [Mismatch] Json
verify = vValue []

vValue
  :: [Text]
  -> Fix (CustomF TypeDefn)
  -> Json
  -> Validation [Mismatch] Json
vValue path pat val = case (pat, val) of
  (Fix (J    NullF       ), Fix    NullF    ) -> pure val
  (Fix (J    TrueF       ), Fix    TrueF    ) -> pure val
  (Fix (J   FalseF       ), Fix   FalseF    ) -> pure val
  (Fix (J (  TextF   p_x)), Fix   (TextF  x)) | p_x == x -> pure val
  (Fix (J (   IntF   p_n)), Fix    (IntF  n)) | p_n == n -> pure val
  (Fix (J (DoubleF   p_n)), Fix (DoubleF  n)) | p_n ~== n -> pure val
  (Fix (J (DoubleF   p_n)), Fix    (IntF  n)) | p_n == fromIntegral n -> pure (Fix (DoubleF p_n))
  (Fix (J (ObjectF   p_o)), Fix (ObjectF  o)) -> Fix . ObjectF <$> vObject path p_o  o
  (Fix (J ( ArrayF  p_xs)), Fix  (ArrayF xs)) -> Fix . ArrayF  <$> vArray  path p_xs xs
  (Fix (C         (_, ty)), Fix            _) -> vTypeDefn path ty val
  (_, _) -> Failure [WrongValue path pat val]

vObject
  :: [Text]
  -> Map Text (Fix (CustomF TypeDefn))
  -> Map Text Json
  -> Validation [Mismatch] (Map Text Json)
vObject path p_o o = Map.fromList <$> checked <* checkUnexpected <* checkMissing
  where
    checked = for (Set.toList present_keys) $ \k ->
      (k,) <$> vValue (path ++ [k]) (p_o Map.! k) (o Map.! k)
    o_keys          = Map.keysSet        o
    p_o_keys        = Map.keysSet      p_o
    missing_keys    = Set.difference   p_o_keys   o_keys
    unexpected_keys = Set.difference     o_keys p_o_keys
    present_keys    = Set.intersection p_o_keys   o_keys
    checkUnexpected =
      unless (Set.null unexpected_keys) $
        Failure [UnexpectedKeys path (Set.toList unexpected_keys)]
    checkMissing    =
      unless (Set.null missing_keys) $
        Failure [MissingKeys path (Set.toList missing_keys)]

vArray
  :: [Text]
  -> [Fix (CustomF TypeDefn)]
  -> [Json]
  -> Validation [Mismatch] [Json]
vArray path p_xs xs = checkElems <* checkLength
  where
    checkElems =
      traverse (\(i, p_x, x) -> vValue (path ++ [fromString (show i)]) p_x x)
        (zip3 [(0::Int)..] p_xs xs)
    checkLength =
      let p_xs_len = length p_xs
          xs_len   = length xs
      in unless (p_xs_len == xs_len) $
           Failure [ArrayLengthMismatch path p_xs_len xs_len]


-- CUSTOM PATTERNS

vTypeDefn
  :: [Text]
  -> TypeDefn
  -> Json
  -> Validation [Mismatch] Json
vTypeDefn path p_val val = case p_val of
  ty -> goTypeDefn ty val
  where
    goTypeDefn :: TypeDefn -> Json -> Validation [Mismatch] Json
    goTypeDefn  TyAny       x@_                 = pure x
    goTypeDefn  TyText      x@(Fix (TextF _))   = pure x
    goTypeDefn  TyInt       x@(Fix (IntF _))    = pure x
    goTypeDefn  TyDouble    x@(Fix (DoubleF _)) = pure x
    goTypeDefn  TyDouble      (Fix (IntF n))    = pure (Fix (DoubleF (fromIntegral n)))
    goTypeDefn  TyBool      x@(Fix TrueF)       = pure x
    goTypeDefn  TyBool      x@(Fix FalseF)      = pure x
    goTypeDefn  TyObject    x@(Fix (ObjectF _)) = pure x
    goTypeDefn  TyArray     x@(Fix (ArrayF _))  = pure x
    goTypeDefn (TyMaybe  _) x@(Fix NullF)       = pure x
    goTypeDefn (TyMaybe ty) x                   = goTypeDefn ty x
    goTypeDefn          ty  x                   = Failure [WrongType path ty x]
