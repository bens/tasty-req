{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Req.PPrint.JSON
  ( ppJson, ppCustomOnly, ppPattern
  , ppObject, ppArray
  ) where

import Control.Recursion (Fix(..), cata)
import Data.List         (intersperse)
import Data.Map          (Map)
import Data.Text         (Text)
import Text.PrettyPrint  (($$), (<+>))

import qualified Data.Map         as Map
import qualified Data.Text        as Text
import qualified Text.PrettyPrint as PP

import Test.Tasty.Req.Types

-- PRETTY PRINTING

ppJson :: Json -> PP.Doc
ppJson = cata ppJsonF

ppCustomOnly :: (a -> PP.Doc) -> Fix (CustomF a) -> PP.Doc
ppCustomOnly = cata . ppCustomF

ppPattern :: (a -> PP.Doc) -> Pattern a -> PP.Doc
ppPattern = cata . ppPatternF

-- PRETTY PRINTING ALGEBRAS

ppJsonF :: JsonF PP.Doc -> PP.Doc
ppJsonF = \case
  NullF     -> PP.text "null"
  TrueF     -> PP.text "true"
  FalseF    -> PP.text "false"
  TextF   x -> PP.text (show x)
  IntF    n -> PP.text (show n)
  DoubleF n -> PP.text (show n)
  ObjectF o -> ppObject o
  ArrayF xs -> ppArray xs

ppCustomF :: (a -> PP.Doc) -> CustomF a PP.Doc -> PP.Doc
ppCustomF pp = \case
  C (nm, x) ->
    PP.braces $ (PP.colon <> PP.text (Text.unpack nm)) PP.<+> pp x
  J j ->
    ppJsonF j

ppPatternF :: (a -> PP.Doc) -> PatternF a PP.Doc -> PP.Doc
ppPatternF pp (M x xs) =
  PP.vcat (intersperse (PP.text "<>") (map (ppCustomF pp) (x:xs)))

ppObject :: Map Text PP.Doc -> PP.Doc
ppObject m = PP.braces (PP.nest 2 (foldr (.) id docs mempty))
  where
    docs = intersperse (<> PP.comma) $ map f (Map.toList m)
    f (k, v) = (PP.$$ ((PP.text (show k) <> PP.colon) PP.<+> v))

ppArray :: [PP.Doc] -> PP.Doc
ppArray xs = PP.brackets (PP.vcat $ PP.punctuate PP.comma xs)
