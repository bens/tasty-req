{-# LANGUAGE TupleSections #-}

module Test.Tasty.Req.Verify
  ( Mismatch(..), verify
  ) where

import Data.String               (fromString)
import Data.Text                 (Text)
import Data.Validation           (Validation(Failure))
import Data.Void                 (Void, absurd)

import qualified Data.Map        as Map

import Test.Tasty.Req.Types      (TypeDefn(..), VoidF, absurdF)
import Test.Tasty.Req.Parse.JSON (Value(..), Object(..))

import qualified Test.Tasty.Req.Parse.JSON as Json

data Mismatch
  = WrongValue    [Text] (Json.Value VoidF TypeDefn) (Json.Value VoidF Void)
  | WrongType     [Text] TypeDefn (Json.Value VoidF Void)
  | UnexpectedKey [Text] Text
  | MissingKeys   [Text] [Text]
    deriving (Eq, Show)

verify
  :: Json.Value VoidF TypeDefn
  -> Json.Value VoidF Void
  -> Validation [Mismatch] (Json.Value VoidF Void)
verify = goValue []
  where
    goValue
      :: [Text]
      -> Json.Value VoidF TypeDefn
      -> Json.Value VoidF Void
      -> Validation [Mismatch] (Json.Value VoidF Void)
    goValue path pat val = case (pat, val) of
      (JsonNull       , JsonNull      ) -> pure val
      (JsonTrue       , JsonTrue      ) -> pure val
      (JsonFalse      , JsonFalse     ) -> pure val
      (JsonText    p_x, JsonText     x) | p_x == x -> pure val
      (JsonInteger p_n, JsonInteger  n) | p_n == n -> pure val
      (JsonDouble  p_n, JsonDouble   n) | p_n == n -> pure val
      (JsonDouble  p_n, JsonInteger  n) | p_n == fromIntegral n -> pure (JsonDouble p_n)
      (JsonObject  p_o, JsonObject   o) -> JsonObject <$> goObject path p_o o
      (JsonArray  p_xs, JsonArray   xs) -> JsonArray <$> goArray path p_xs xs
      (JsonCustom _ ty,              _) -> goWildcard path ty val
      (              _, JsonCustom _ x) -> absurd x
      (JsonCombine p_x,              _) -> absurdF p_x
      (              _, JsonCombine  x) -> absurdF x
      (              _,              _) -> Failure [WrongValue path pat val]

    goObject
      :: [Text]
      -> Json.Object VoidF TypeDefn
      -> Json.Object VoidF Void
      -> Validation [Mismatch] (Json.Object VoidF Void)
    goObject path (Object p_o) (Object o) =
      Object . Map.fromList <$> checked <* missings
      where
        checked  = traverse f (Map.toList o)
        f (k, v) = case Map.lookup k p_o of
          Nothing  -> Failure [UnexpectedKey path k]
          Just p_v -> (k,) <$> goValue (k:path) p_v v
        missings =
          let filtered = Map.filterWithKey (\k _ -> k `notElem` Map.keys o) p_o
          in if Map.null filtered
               then pure ()
               else Failure [MissingKeys path (Map.keys filtered)]

    goArray
      :: [Text]
      -> [Json.Value VoidF TypeDefn]
      -> [Json.Value VoidF Void]
      -> Validation [Mismatch] [Json.Value VoidF Void]
    goArray path p_xs xs =
        traverse (\(i, p_x, x) -> goValue (fromString (show i):path) p_x x)
          (zip3 [(0::Int)..] p_xs xs)

    goWildcard
      :: [Text]
      -> TypeDefn
      -> Json.Value VoidF Void
      -> Validation [Mismatch] (Json.Value VoidF Void)
    goWildcard _     TyAny       x                = pure x
    goWildcard _     TyString    x@JsonText   {}  = pure x
    goWildcard _     TyInteger   x@JsonInteger{}  = pure x
    goWildcard _     TyDouble    x@JsonDouble {}  = pure x
    goWildcard _     TyDouble     (JsonInteger n) = pure (JsonDouble (fromIntegral n))
    goWildcard _     TyBool      x@JsonTrue       = pure x
    goWildcard _     TyBool      x@JsonFalse      = pure x
    goWildcard _     TyObject    x@JsonObject{}   = pure x
    goWildcard _     TyArray     x@JsonArray{}    = pure x
    goWildcard _    (TyMaybe  _) x@JsonNull       = pure x
    goWildcard path (TyMaybe ty) x                = goWildcard path ty x
    goWildcard path          ty  x                = Failure [WrongType path ty x]
