{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Tasty.Req.Parse
  ( parser
  ) where

import Control.Applicative (many, optional, some, (<|>))
import Data.Char           (isAlphaNum, isUpper)
import Data.Foldable       (asum)
import Data.Text           (Text)

import qualified Data.Set                   as Set
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P.C
import qualified Text.Megaparsec.Char.Lexer as P.L

import Test.Tasty.Req.Parse.Common
  (ensure1stCol, hspace, lexemeH, spaces, symbol, symbolH)
import Test.Tasty.Req.Types

import qualified Test.Tasty.Req.Parse.JSON as Json

parser :: Ord e => P.ParsecT e Text m [Command]
parser = spaces *> many pCommand <* P.eof

-- REF

pRef :: Ord e => P.ParsecT e Text m Ref
pRef = do
  r <- lexemeH P.L.decimal
  side <- (Request <$ P.C.char '?') <|> (Response <$ P.C.char '!')
  path <- lexemeH $ many $ P.between (P.C.char '[') (P.C.char ']') $
    P.eitherP
      (Json.runParser Json.combineVoidF Json.integer')  -- array index
      (Json.runParser Json.combineVoidF Json.text')     -- object key
  let psr = Json.runParser Json.combineVoidF Json.text'
  sans <- (symbol "/" *> many (lexemeH psr)) <|> pure []
  pure (Ref r side path (Set.fromList sans))

-- GENERATOR

pGenerator :: Ord e => P.ParsecT e Text m Generator
pGenerator = term <|> (GenMaybe <$> (symbolH "maybe" *> term))
  where
    term = asum
      [ GenString  <$ symbolH "string"
      , GenInteger <$ symbolH "integer"
      , GenDouble  <$ symbolH "double"
      , GenBool    <$ symbolH "boolean"
      ]

-- TYPE DEFN

pTypeDefn :: Ord e => P.ParsecT e Text m TypeDefn
pTypeDefn = term <|> (TyMaybe <$> (symbolH "maybe" *> term))
  where
    term = asum
      [ TyAny     <$ symbolH "any"
      , TyString  <$ symbolH "string"
      , TyInteger <$ symbolH "integer"
      , TyDouble  <$ symbolH "double"
      , TyBool    <$ symbolH "boolean"
      , TyObject  <$ symbolH "object"
      , TyArray   <$ symbolH "array"
      ]

-- COMMAND

pMethod :: Ord e => P.ParsecT e Text m Text
pMethod = lexemeH $
  P.takeWhile1P (Just "HTTP method (GET, POST, ...)") isUpper

pUrl :: Ord e => P.ParsecT e Text m [Either Ref Text]
pUrl = lexemeH (some pSegment)
  where
    pSegment = P.eitherP (P.between (symbolH "{:r") (P.C.char '}') pRef) pTextSegment
    pTextSegment =
      P.takeWhile1P (Just "URL text segment") $ \x ->
        isAlphaNum x || x `elem` ("/-_%?&=[]" :: String)

pCommand :: Ord e => P.ParsecT e Text m Command
pCommand = do
  n       <- ensure1stCol *> P.L.decimal
  always  <- ((True <$ P.C.char '*') <|> pure False) <* symbolH ":"
  method  <- pMethod
  url     <- pUrl <* hspace <* P.C.newline
  request <- optional $
    Json.runParser Json.combineList $
      let custom = [ ("r", ReqRef <$> pRef)
                   , ("g", ReqGen <$> pGenerator)
                   ]
      in Json.combine
         ( Json.withCustom (Json.customParsers custom) Json.object <>
           Json.withCustom (Json.customParsers custom) Json.custom
         ) <>
         Json.withCustom (Json.customParsers custom) Json.array
  spaces *> ensure1stCol *> symbol "---"
  expected <- optional $
    Json.runParser Json.combineList $
      let customA = [ ("r",    RespRef       <$> pRef)
                    , ("t",    RespTypeDefn  <$> pTypeDefn)
                    ]
          customB = [ ("r", RespRef <$> pRef)
                    ]
      in Json.combine
         ( Json.withCustom (Json.customParsers customA) Json.object <>
           Json.withCustom (Json.customParsers customB) Json.custom
         ) <>
         Json.withCustom (Json.customParsers customA) Json.array
  pure (Command n always method url request expected)
