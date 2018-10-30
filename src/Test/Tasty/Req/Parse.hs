{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Test.Tasty.Req.Parse
  ( parser
  ) where

import Control.Applicative                  ((<|>), many, optional, some)
import Data.Char                            (isAlphaNum, isUpper)
import Data.Foldable                        (asum)
import Data.Maybe                           (fromMaybe)
import Data.Text                            (Text)

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P.C
import qualified Text.Megaparsec.Char.Lexer as P.L

import Test.Tasty.Req.Parse.Common
import qualified Test.Tasty.Req.Parse.JSON  as Json
import Test.Tasty.Req.Types

parser :: Ord e => P.ParsecT e Text m [Command]
parser = spaces *> many pCommand <* P.eof

-- REF

pRef :: Ord e => P.ParsecT e Text m Ref
pRef = do
  r <- lexemeH P.L.decimal
  side <- (Request <$ P.C.char '?') <|> (Response <$ P.C.char '!')
  path <- many $ P.between (P.C.char '[') (P.C.char ']') (Json.runParser Json.text')
  pure (Ref r side path)

-- TYPE DEFN

pTypeDefn :: Ord e => P.ParsecT e Text m TypeDefn
pTypeDefn = term <|> (TyMaybe <$> (symbolH "maybe" *> term))
  where
    term = asum
      [ TyString  <$ symbolH "string"
      , TyInteger <$ symbolH "integer"
      , TyDouble  <$ symbolH "double"
      , TyBool    <$ symbolH "boolean"
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
  let requestPsr = Json.runParser $
        Json.withCustom (Json.customParsers [("r", pRef)]) Json.object <>
        Json.withCustom (Json.customParsers [("r", pRef)]) Json.custom
      expectedPsr = Json.runParser $
        Json.withCustom (Json.customParsers [("r", Left <$> pRef), ("t", Right <$> pTypeDefn)]) Json.object <>
        Json.withCustom (Json.customParsers [("r", Left <$> pRef)]) Json.custom
  n       <- ensure1stCol *> P.L.decimal
  always  <- ((True <$ P.C.char '*') <|> pure False) <* symbolH ":"
  method  <- pMethod
  url     <- pUrl <* hspace <* P.C.newline
  request <- optional (P.sepBy1 requestPsr (symbol "<>"))
  spaces *> ensure1stCol *> symbol "---"
  expected <- P.sepBy expectedPsr (symbol "<>")
  pure (Command n always method url (fromMaybe [] request) expected)
