{-# LANGUAGE LambdaCase #-}

module Test.Tasty.Req.PPrint
  ( ppCommand
  ) where

import Text.PrettyPrint ((<+>))

import qualified Data.Set         as Set
import qualified Data.Text        as Text
import qualified Text.PrettyPrint as PP

import Test.Tasty.Req.PPrint.JSON
import Test.Tasty.Req.Types

ppCommand :: Command -> PP.Doc
ppCommand x =
  PP.vcat
    [ (ppId <> ppAlways <> PP.text ":") <+> ppMethod <+> ppUrl
    , ppReq, PP.text "---", ppResp
    ]
  where
    ppId     = PP.text (show (command'id x))
    ppAlways = if command'always x then PP.char '*' else mempty
    ppMethod = PP.text (Text.unpack (command'method x))
    ppUrl    = mconcat (map (either ppRef (PP.text . Text.unpack)) (command'url x))
    ppReq    = maybe mempty (ppPattern ppReqCustom)  (command'request_body  x)
    ppResp   = maybe mempty (ppPattern ppRespCustom) (command'response_body x)

ppReqCustom :: ReqCustom -> PP.Doc
ppReqCustom = \case
  ReqRef r -> ppRef r
  ReqGen g -> ppGenerator g

ppRespCustom :: RespCustom -> PP.Doc
ppRespCustom = \case
  RespRef r      -> ppRef r
  RespTypeDefn t -> ppTypeDefn t

ppRef :: Ref -> PP.Doc
ppRef (Ref i side path excls) =
  (PP.text (show i) <> ppSide <> ppPath) <+> ppExcls
  where
    ppSide = PP.char $ if side == Request then '?' else '!'
    ppPath = PP.cat $ map (PP.brackets . PP.text . either show show) path
    ppExcls
      | null excls = mempty
      | otherwise = PP.char '/' <+> PP.hsep (map (PP.text . show) (Set.toList excls))

ppGenerator :: Generator -> PP.Doc
ppGenerator = \case
  GenText    -> PP.text "string"
  GenInt     -> PP.text "integer"
  GenDouble  -> PP.text "double"
  GenBool    -> PP.text "boolean"
  GenMaybe g -> PP.text "maybe" <+> ppGenerator g

ppTypeDefn :: TypeDefn -> PP.Doc
ppTypeDefn = \case
  TyAny     -> PP.text "any"
  TyText    -> PP.text "string"
  TyInt     -> PP.text "integer"
  TyDouble  -> PP.text "double"
  TyBool    -> PP.text "boolean"
  TyObject  -> PP.text "object"
  TyArray   -> PP.text "array"
  TyMaybe t -> PP.text "maybe" <+> ppTypeDefn t
