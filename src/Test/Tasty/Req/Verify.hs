{-# LANGUAGE TupleSections #-}

module Test.Tasty.Req.Verify
  ( Mismatch(..), verify
  ) where

import Control.Monad             (unless)
import Data.AEq                  ((~==))
import Data.String               (fromString)
import Data.Text                 (Text)
import Data.Traversable          (for)
import Data.Validation           (Validation(Failure))
import Data.Void                 (Void, absurd)

import qualified Data.Map        as Map
import qualified Data.Set        as Set

import Test.Tasty.Req.Parse.JSON (Value(..), Object(..))
import Test.Tasty.Req.Types      (TypeDefn(..), VoidF, absurdF)

import qualified Test.Tasty.Req.Parse.JSON as Json

data Mismatch
  = WrongValue          [Text] (Json.Value VoidF (Either () TypeDefn)) (Json.Value VoidF Void)
  | WrongType           [Text] TypeDefn (Json.Value VoidF Void)
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
  :: Json.Value VoidF (Either () TypeDefn)
  -> Json.Value VoidF Void
  -> Validation [Mismatch] (Json.Value VoidF Void)
verify = vValue []

vValue
  :: [Text]
  -> Json.Value VoidF (Either () TypeDefn)
  -> Json.Value VoidF Void
  -> Validation [Mismatch] (Json.Value VoidF Void)
vValue path pat val = case (pat, val) of
  (JsonNull       , JsonNull      ) -> pure val
  (JsonTrue       , JsonTrue      ) -> pure val
  (JsonFalse      , JsonFalse     ) -> pure val
  (JsonText    p_x, JsonText     x) | p_x == x -> pure val
  (JsonInteger p_n, JsonInteger  n) | p_n == n -> pure val
  (JsonDouble  p_n, JsonDouble   n) | p_n ~== n -> pure val
  (JsonDouble  p_n, JsonInteger  n) | p_n == fromIntegral n -> pure (JsonDouble p_n)
  (JsonObject  p_o, JsonObject   o) -> JsonObject <$> vObject path p_o  o
  (JsonArray  p_xs, JsonArray   xs) -> JsonArray  <$> vArray  path p_xs xs
  (JsonCustom _ ty,              _) -> vCustom path ty val
  (              _, JsonCustom _ x) -> absurd x
  (JsonCombine p_x,              _) -> absurdF p_x
  (              _, JsonCombine  x) -> absurdF x
  (              _,              _) -> Failure [WrongValue path pat val]

vObject
  :: [Text]
  -> Json.Object VoidF (Either () TypeDefn)
  -> Json.Object VoidF Void
  -> Validation [Mismatch] (Json.Object VoidF Void)
vObject path (Object p_o) (Object o) =
  Object . Map.fromList <$> checked <* checkUnexpected <* checkMissing
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
  -> [Json.Value VoidF (Either () TypeDefn)]
  -> [Json.Value VoidF Void]
  -> Validation [Mismatch] [Json.Value VoidF Void]
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


-- CUSTOM VALUES

vCustom
  :: [Text]
  -> Either () TypeDefn
  -> Json.Value VoidF Void
  -> Validation [Mismatch] (Json.Value VoidF Void)
vCustom path p_val val = case p_val of
  Left ()  -> goArrayMany () val
  Right ty -> goTypeDefn ty val
  where
    goArrayMany
      :: ()
      -> Json.Value VoidF Void
      -> Validation [Mismatch] (Json.Value VoidF Void)
    goArrayMany () _x = pure undefined

    goTypeDefn
      :: TypeDefn
      -> Json.Value VoidF Void
      -> Validation [Mismatch] (Json.Value VoidF Void)
    goTypeDefn  TyAny       x                = pure x
    goTypeDefn  TyString    x@JsonText   {}  = pure x
    goTypeDefn  TyInteger   x@JsonInteger{}  = pure x
    goTypeDefn  TyDouble    x@JsonDouble {}  = pure x
    goTypeDefn  TyDouble     (JsonInteger n) = pure (JsonDouble (fromIntegral n))
    goTypeDefn  TyBool      x@JsonTrue       = pure x
    goTypeDefn  TyBool      x@JsonFalse      = pure x
    goTypeDefn  TyObject    x@JsonObject{}   = pure x
    goTypeDefn  TyArray     x@JsonArray{}    = pure x
    goTypeDefn (TyMaybe  _) x@JsonNull       = pure x
    goTypeDefn (TyMaybe ty) x                = goTypeDefn ty x
    goTypeDefn          ty  x                = Failure [WrongType path ty x]
