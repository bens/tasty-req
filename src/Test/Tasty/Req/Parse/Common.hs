{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Req.Parse.Common
  ( hspace, hspace1
  , spaces, spacesH
  , lexeme, lexemeH
  , symbol, symbolH
  , ensure1stCol
  ) where

import Control.Applicative (empty)
import Control.Monad       (unless)
import Data.List.NonEmpty  (NonEmpty((:|)))
import Data.Text           (Text)

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P.C
import qualified Text.Megaparsec.Char.Lexer as P.L

hspace :: Ord e => P.ParsecT e Text m ()
hspace = () <$ P.takeWhileP Nothing (`elem` (" \t" :: String))

hspace1 :: Ord e => P.ParsecT e Text m ()
hspace1 = () <$ P.takeWhile1P Nothing (`elem` (" \t" :: String))

spaces :: Ord e => P.ParsecT e Text m ()
spaces = P.L.space P.C.space1 (P.L.skipLineComment "#") empty

spacesH :: Ord e => P.ParsecT e Text m ()
spacesH = P.L.space hspace1 (P.L.skipLineComment "#") empty

lexeme :: Ord e => P.ParsecT e Text m a -> P.ParsecT e Text m a
lexeme = P.L.lexeme spaces

lexemeH :: Ord e => P.ParsecT e Text m a -> P.ParsecT e Text m a
lexemeH = P.L.lexeme spacesH

symbol :: Ord e => Text -> P.ParsecT e Text m ()
symbol x = () <$ P.L.symbol spaces x

symbolH :: Ord e => Text -> P.ParsecT e Text m ()
symbolH x = () <$ P.L.symbol spacesH x

ensure1stCol :: Ord e => P.ParsecT e Text m ()
ensure1stCol = do
  pos <- P.getSourcePos
  let col = P.sourceColumn pos
  unless (col == P.mkPos 1) $
    P.unexpected (P.Label ('s' :| "pace at the beginning of line"))
