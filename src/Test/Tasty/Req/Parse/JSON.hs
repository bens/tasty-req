{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Test.Tasty.Req.Parse.JSON
  ( -- * JSON Values
    Value(..), ppValue, substitute, discardCustom
    -- * JSON Objects
  , Object(..), ppObject
  -- * Parsers
  , Parser, runParser
  -- * Custom Parsers
  , CustomParser, customParsers, emptyCustom, withCustom
  -- * JsonValue parsers
  , value
  , object, array, text, number, atom, custom
  -- * Type-specific parsers
  , object', array', text', number', custom'
  ) where

import Control.Applicative
import Control.Monad                  (void)
import Data.Char                      (chr, isDigit, isHexDigit, isLower, isUpper, ord)
import Data.Foldable                  (asum)
import Data.List                      (foldl', intersperse)
import Data.Map                       (Map)
import Data.Text                      (Text)

import qualified Data.Map             as Map
import qualified Data.Text            as Text
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.PrettyPrint     as PP

import Test.Tasty.Req.Parse.Common

newtype CustomParser m e x = CustomParser (P.ParsecT e Text m (Text, x))

emptyCustom :: Ord e => CustomParser m e x
emptyCustom = customParsers []

customParsers :: Ord e => [(Text, P.ParsecT e Text m x)] -> CustomParser m e x
customParsers ps = CustomParser $
  asum [(ty,) <$> (symbol (":" <> ty) *> psr) | (ty, psr) <- ps]

data Value a
  = JsonNull
  | JsonTrue
  | JsonFalse
  | JsonText Text
  | JsonInteger Int
  | JsonDouble Double
  | JsonObject (Object a)
  | JsonArray [Value a]
  | JsonCustom Text a
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

discardCustom :: Value a -> Value ()
discardCustom = void

newtype Object a
  = Object{ getObject :: Map Text (Value a) }
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (Object a) where
  Object a <> Object b = Object (a <> b)

instance Monoid (Object a) where
  mempty = Object mempty
  mappend = (<>)

ppValue :: (a -> PP.Doc) -> Value a -> PP.Doc
ppValue f = \case
  JsonNull        -> PP.text "null"
  JsonTrue        -> PP.text "true"
  JsonFalse       -> PP.text "false"
  JsonText x      -> PP.text (show x)
  JsonInteger n   -> PP.text (show n)
  JsonDouble n    -> PP.text (show n)
  JsonObject o    -> ppObject f o
  JsonArray xs    -> ppArray f xs
  JsonCustom nm x -> PP.braces $ (PP.colon <> PP.text (Text.unpack nm)) PP.<+> f x

ppObject :: (a -> PP.Doc) -> Object a -> PP.Doc
ppObject f (Object m) =
  PP.braces (PP.nest 2 (foldr (.) id (addCommas fields) mempty))
  where
    fields    = map (uncurry go) (Map.toList m)
    go k v    = (PP.$$ ((PP.text (show k) <> PP.colon) PP.<+> ppValue f v))
    addCommas = intersperse (<> PP.comma)

ppArray :: (a -> PP.Doc) -> [Value a] -> PP.Doc
ppArray f xs =
  PP.brackets (PP.nest 2 (foldr (.) id (addCommas items) mempty))
  where
    items     = map go xs
    go x      = (ppValue f x PP.$$)
    addCommas = intersperse (<> PP.comma)

substitute :: Monad m => (Text -> a -> m (Value b)) -> Value a -> m (Value b)
substitute f = \case
  JsonNull              -> pure JsonNull
  JsonTrue              -> pure JsonTrue
  JsonFalse             -> pure JsonFalse
  JsonText x            -> pure (JsonText x)
  JsonInteger x         -> pure (JsonInteger x)
  JsonDouble x          -> pure (JsonDouble x)
  JsonObject (Object o) -> JsonObject . Object <$> traverse (substitute f) o
  JsonArray xs          -> JsonArray <$> traverse (substitute f) xs
  JsonCustom ty x       -> f ty x

data Parser m e x a
  = Parser
      (Map Char [CustomParser m e x -> P.ParsecT e Text m a])
      (CustomParser m e x -> P.ParsecT e Text m a)
    deriving Functor

instance Ord e => Semigroup (Parser m e x a) where
  Parser a x <> Parser b y = Parser (Map.unionWith (++) a b) (\c -> x c <|> y c)

instance Ord e => Monoid (Parser m e x a) where
  mempty = Parser Map.empty (const empty)
  mappend = (<>)

runParser :: Ord e => Parser m e x a -> P.ParsecT e Text m a
runParser = runParser' emptyCustom

runParser' :: Ord e => CustomParser m e x -> Parser m e x a -> P.ParsecT e Text m a
runParser' c (Parser ms p) =
  (lexeme (P.oneOf (Map.keys ms)) >>= \x -> asum (map ($ c) (ms Map.! x))) <|> p c

withCustom :: CustomParser m e x -> Parser m e x a -> Parser m e x a
withCustom c (Parser x y) = Parser (fmap (map (\k _ -> k c)) x) (\_ -> y c)

prefixParser
  :: Ord e
  => Char
  -> (CustomParser m e x -> P.ParsecT e Text m a)
  -> Parser m e x a
prefixParser c m = Parser (Map.singleton c [m]) (const empty)

otherParser
  :: (CustomParser m e x -> P.ParsecT e Text m a)
  -> Parser m e x a
otherParser = Parser Map.empty


-- Value Parsers

value :: Ord e => Parser m e x (Value x)
value =
  mconcat [atom, text, number, object, array, custom]

object :: Ord e => Parser m e x (Value x)
object = JsonObject <$> object'

array :: Ord e => Parser m e x (Value x)
array = JsonArray <$> array'

text :: Ord e => Parser m e x (Value x)
text = JsonText <$> text'

number :: Ord e => Parser m e x (Value x)
number = either JsonInteger JsonDouble <$> number'

atom :: Ord e => Parser m e x (Value x)
atom = mconcat
  [ otherParser (const (JsonNull  <$ symbol "null"))
  , otherParser (const (JsonTrue  <$ symbol "true"))
  , otherParser (const (JsonFalse <$ symbol "false"))
  ]

custom :: Ord e => Parser m e x (Value x)
custom = uncurry JsonCustom <$> custom'


-- Typeful Parsers

object' :: Ord e => Parser m e x (Object x)
object' = prefixParser '{' $ \c -> do
  obj <- flip P.sepEndBy (symbol ",") $
    (,) <$> (lexeme (runParser' c text') <* symbol ":")
        <*> lexeme (runParser' c value)
  Object (Map.fromList obj) <$ symbol "}"

array' :: Ord e => Parser m e x [Value x]
array' = prefixParser '[' $ \c ->
  P.sepBy (lexeme (runParser' c value)) (symbol ",") <* symbol "]"

text' :: Ord e => Parser m e x Text
text' = otherParser $ \_ -> P.label "string literal" $ P.C.char '"' *> go id
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
        _ -> error "impossible"

number' :: Ord e => Parser m e x (Either Int Double)
number' = otherParser $ \_ -> P.label "number" $ do
  neg <- (True <$ P.C.char '-') <|> pure False
  as <- integer <|> (0 <$ P.C.char '0')
  (Right . (if neg then negate else id) <$> double (fromIntegral as)) <|>
    pure (Left ((if neg then negate else id) as))
  where
    val x = ord x - 48
    integer :: (Ord e) => P.ParsecT e Text m Int
    integer = do
      digits <- (:) <$> P.oneOf ['1'..'9'] <*> many P.C.digitChar
      pure $! foldl' (\i x -> i * 10 + val x) 0 digits
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

custom' :: Ord e => Parser m e x (Text, x)
custom' = prefixParser '{' $ \(CustomParser c) -> c <* symbol "}"


-- Helpers

parseHex :: String -> Maybe Int
parseHex = foldl' (liftA2 (\i x -> i * 16 + x)) (Just 0) . map f
  where
    f x | isDigit x            = Just (ord x - 48)
        | isUpper x && x < 'G' = Just (ord x - 55)
        | isLower x && x < 'g' = Just (ord x - 87)
        | otherwise            = Nothing
