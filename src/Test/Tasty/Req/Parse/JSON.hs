{- |
  This JSON parser has a few extensions, it will parse and ignore comments, it
  allows custom parsers of user-defined types (the 'x' parameter), and has a
  syntax for merging JSON object together (the 'f' parameter).  The custom
  parsers and merging can both be statically disallowed by instantiating the two
  type parameters respectively to 'Void' and 'VoidF'.
-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Test.Tasty.Req.Parse.JSON
  ( -- * Parsers
    Parser, Ctx(..), runParser, runParserJSON, runParserPattern, mapParser
    -- * Custom Parsers
  , CustomParser, customParsers, emptyCustom, withCustom
    -- * JsonValue parsers
  , json, atom, text, number, object, array
    -- * Extras
  , custom, combine
    -- * Typeful parsers
  , text', integer', number'
  ) where

import Control.Applicative (empty, liftA2, many, some, (<|>))
import Control.Recursion   (Fix(..))
import Data.Char           (chr, isDigit, isHexDigit, isLower, isUpper, ord)
import Data.Foldable       (asum)
import Data.List           (foldl', sort)
import Data.Map            (Map)
import Data.Text           (Text)

import qualified Data.Map             as Map
import qualified Data.Text            as Text
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P.C

import Test.Tasty.Req.Parse.Common (lexeme, symbol)
import Test.Tasty.Req.Types

-- PARSER

data Ctx m e f x = Ctx
  { injectJsonF   :: JsonF (Fix f) -> Fix f
  , injectCustomF :: Maybe (Text -> x -> Fix f)
  , injectMergeF  :: Maybe (Fix f -> [Fix f] -> Fix f)
  , customParser  :: CustomParser m e x
  }

data Parser m e f x a
  = Parser
      (Map Char [Ctx m e f x -> P.ParsecT e Text m a])
      (Ctx m e f x -> P.ParsecT e Text m a)
    deriving Functor

instance Ord e => Semigroup (Parser m e f x a) where
  Parser a x <> Parser b y =
    Parser (Map.unionWith (++) a b) (\ctx -> x ctx <|> y ctx)

instance Ord e => Monoid (Parser m e f x a) where
  mempty = Parser Map.empty (const empty)
  mappend = (<>)

runParser
  :: Ord e
  => (JsonF (Fix f) -> Fix f)          -- ^ Inject JsonF values
  -> Maybe (Text -> x -> Fix f)        -- ^ Inject Custom values
  -> Maybe (Fix f -> [Fix f] -> Fix f) -- ^ Inject combinations
  -> Parser m e f x a
  -> P.ParsecT e Text m a
runParser f g h = runParser' (Ctx f g h emptyCustom)

runParserJSON
  :: Ord e
  => Parser m e JsonF x a
  -> P.ParsecT e Text m a
runParserJSON = runParser Fix Nothing Nothing

runParserPattern
  :: Ord e
  => Parser m e (PatternF x) x a
  -> P.ParsecT e Text m a
runParserPattern = runParser injJ (Just injC) (Just injM)
  where
    injJ x                 = Fix (M (J x) [])
    injC nm x              = Fix (M (C (nm, x)) [])
    injM (Fix (M x xs)) ys = Fix (M x (xs ++ (ys >>= collect)))
    collect (Fix (M x xs)) = x:xs

runParser'
  :: Ord e
  => Ctx m e f x
  -> Parser m e f x a
  -> P.ParsecT e Text m a
runParser' ctx (Parser prefixed other) =
  (pKey >>= \i -> asum [p ctx | p <- prefixed Map.! i]) <|> other ctx
  where
    pKey = lexeme (P.oneOf (Map.keys prefixed))

prefixParser
  :: Ord e
  => Char
  -> (Ctx m e f x -> P.ParsecT e Text m a)
  -> Parser m e f x a
prefixParser ctx m = Parser (Map.singleton ctx [m]) (const empty)

otherParser
  :: (Ctx m e f x -> P.ParsecT e Text m a)
  -> Parser m e f x a
otherParser = Parser Map.empty

mapParser :: (Ctx m e f x -> a -> b)-> Parser m e f x a -> Parser m e f x b
mapParser f (Parser prefixed other) = Parser prefixed' other'
  where
    prefixed' = fmap (map (\k ctx -> fmap (f ctx) (k ctx))) prefixed
    other' ctx = fmap (f ctx) (other ctx)

-- CUSTOM VALUES

newtype CustomParser m e x = CustomParser (Map Text (P.ParsecT e Text m x))

emptyCustom :: CustomParser m e x
emptyCustom = customParsers []

customParsers :: [(Text, P.ParsecT e Text m x)] -> CustomParser m e x
customParsers = CustomParser . Map.fromList

withCustom :: CustomParser m e x -> Parser m e f x a -> Parser m e f x a
withCustom cust (Parser prefixed other) =
  Parser
    (fmap (map (\k ctx -> k (addCustom ctx cust))) prefixed)
    (\ctx -> other (addCustom ctx cust))
  where
    addCustom ctx (CustomParser b) =
      let CustomParser a = customParser ctx
      in ctx{ customParser = CustomParser (a <> b) }

-- VALUE PARSERS

json :: Ord e => Parser m e f x (Fix f)
json = mconcat [atom, text, number, combine (array <> object <> custom)]

atom :: Ord e => Parser m e f x (Fix f)
atom = mapParser injectJsonF $ mconcat
  [ otherParser (const ( NullF <$ symbol "null"))
  , otherParser (const ( TrueF <$ symbol "true"))
  , otherParser (const (FalseF <$ symbol "false"))
  ]

text :: Ord e => Parser m e f x (Fix f)
text = mapParser injectJsonF (TextF <$> text')

number :: Ord e => Parser m e f x (Fix f)
number = mapParser injectJsonF (either IntF DoubleF <$> number')

object :: Ord e => Parser m e f x (Fix f)
object = mapParser injectJsonF (ObjectF <$> object')

array :: Ord e => Parser m e f x (Fix f)
array = mapParser injectJsonF (ArrayF <$> array')

custom :: Ord e => Parser m e f x (Fix f)
custom = prefixParser '{' $ \ctx ->
  case injectCustomF ctx of
    Nothing -> empty
    Just f  -> do
      _ <- P.C.char ':'
      let CustomParser c = customParser ctx
      let p = asum [ (nm,) <$> (symbol nm *> psr)
                   | (nm, psr) <- Map.toList c ]
      uncurry f <$> (p <* symbol "}")

combine :: Ord e => Parser m e f x (Fix f) -> Parser m e f x (Fix f)
combine ps = otherParser $ \ctx -> do
  let psr = runParser' ctx ps
  let psrMerge = (symbol "<>" *> P.sepBy psr (symbol "<>")) <|> pure []
  case injectMergeF ctx of
    Nothing -> psr
    Just f  ->
      (,) <$> psr <*> psrMerge >>= \case
        (x, []) -> pure x
        (x, xs) -> pure (f x xs)


-- TYPEFUL PARSERS

object' :: Ord e => Parser m e f x (Map Text (Fix f))
object' = prefixParser '{' $ \ctx -> do
  pairs <- flip P.sepEndBy (symbol ",") $
    (,) <$> (lexeme (runParser' ctx text') <* symbol ":")
        <*> lexeme (runParser' ctx json)
  let repeats = map fst $
        let ks = sort (map fst pairs)
        in filter (uncurry (==)) (zip ks (drop 1 ks))
  if null repeats
    then Map.fromList pairs <$ symbol "}"
    else fail ("Repeated object keys: " ++ show repeats)

array' :: Ord e => Parser m e f x [Fix f]
array' = prefixParser '[' $ \ctx ->
  P.sepBy (lexeme (runParser' ctx json)) (symbol ",") <* symbol "]"

text' :: Ord e => Parser m e f x Text
text' = otherParser $ \_ -> P.label "string literal" (P.C.char '"' *> go id)
  where
    go :: Ord e => (Text -> Text) -> P.ParsecT e Text m Text
    go k = do
      xs <- P.takeWhileP Nothing (`notElem` ['"', '\\'])
      P.anySingle >>= \case
        '"'  -> pure (k xs)
        '\\' -> P.anySingle >>= \case
          '"'  -> go (k . (xs <>) . ("\"" <>))
          '\\' -> go (k . (xs <>) . ("\\" <>))
          '/'  -> go (k . (xs <>) . ("/"  <>))
          'b'  -> go (k . (xs <>) . ("\b" <>))
          'f'  -> go (k . (xs <>) . ("\f" <>))
          'n'  -> go (k . (xs <>) . ("\n" <>))
          'r'  -> go (k . (xs <>) . ("\r" <>))
          't'  -> go (k . (xs <>) . ("\t" <>))
          'u'  -> fmap parseHex (P.count 4 (P.satisfy isHexDigit)) >>= \case
            Just n  -> go (k . (xs <>) . (Text.pack [chr n] <>))
            Nothing -> fail "failed to parse hex bytes"
          c -> fail ("bad character code: '\\" ++ [c] ++ "'")
        _ -> error "impossible: notElem"

pInteger :: Ord e => P.ParsecT e Text m (Bool, Int)
pInteger = do
  neg <- (True <$ P.C.char '-') <|> pure False
  as <- integer <|> (0 <$ P.C.char '0')
  pure (neg, as)
  where
    val x = ord x - 48
    integer :: (Ord e) => P.ParsecT e Text m Int
    integer = do
      digits <- (:) <$> P.oneOf ['1'..'9'] <*> many P.C.digitChar
      pure $! foldl' (\i x -> i * 10 + val x) 0 digits

integer' :: Ord e => Parser m e f x Int
integer' = otherParser $ \_ -> P.label "integer" $ do
  (neg, n) <- pInteger
  pure $ (if neg then negate else id) n

number' :: Ord e => Parser m e f x (Either Int Double)
number' = otherParser $ \_ -> P.label "number" $ do
  (neg, as) <- pInteger
  (Right . (if neg then negate else id) <$> double (fromIntegral as)) <|>
    pure (Left ((if neg then negate else id) as))
  where
    val x = ord x - 48
    double :: (Ord e) => Double -> P.ParsecT e Text m Double
    double as = decimals as <|> scinot as
    decimals :: (Ord e) => Double -> P.ParsecT e Text m Double
    decimals as = do
      digits <- P.C.char '.' *> some P.C.digitChar
      let bs = 0.1 * foldr (\x i -> i / 10 + fromIntegral (val x)) 0 digits
      scinot (as + bs) <|> pure (as + bs)
    scinot :: (Ord e) => Double -> P.ParsecT e Text m Double
    scinot as = do
      _  <- P.oneOf ['e', 'E']
      op <- asum [(*) <$ P.C.char '+', (/) <$ P.C.char '-', pure (*)]
      digits <- some P.C.digitChar
      let expn = foldl' (\i x -> i * 10 + val x) 0 digits
      pure $! as `op` (10 ^ expn)

-- Helpers

parseHex :: String -> Maybe Int
parseHex = foldl' (liftA2 (\i x -> i * 16 + x)) (Just 0) . map f
  where
    f x | isDigit x            = Just (ord x - 48)
        | isUpper x && x < 'G' = Just (ord x - 55)
        | isLower x && x < 'g' = Just (ord x - 87)
        | otherwise            = Nothing
