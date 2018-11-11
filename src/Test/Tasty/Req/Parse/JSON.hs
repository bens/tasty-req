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
  ( -- * JSON Values
    Value(..), ppValue, substitute
    -- * JSON Objects
  , Object(..), getObject, ppObject
  -- * Parsers
  , Parser, runParser
  -- * Custom Parsers
  , CustomParser, customParsers, emptyCustom, withCustom
  -- * Combiners
  , Combine, combineList, combineVoidF, injectVoidF, elimCombine
  -- * JsonValue parsers
  , value
  , object, array, text, number, atom, combine, custom
  -- * Type-specific parsers
  , object', array', text', integer', number', combine', custom'
  ) where

import Control.Applicative
import Control.Monad       (foldM)
import Data.Char           (chr, isDigit, isHexDigit, isLower, isUpper, ord)
import Data.Foldable       (asum, toList)
import Data.List           (foldl', intersperse, sort)
import Data.List.NonEmpty  (NonEmpty((:|)))
import Data.Map            (Map)
import Data.Maybe          (fromMaybe)
import Data.Semigroup      (sconcat)
import Data.Text           (Text)

import qualified Data.Map             as Map
import qualified Data.Text            as Text
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P.C
import qualified Text.PrettyPrint     as PP

import Test.Tasty.Req.Parse.Common
import Test.Tasty.Req.Types


-- JSON VALUES

substitute
  :: (Monad m, Traversable f)
  => (Text -> a -> m (Value f b))
  -> Value f a -> m (Value f b)
substitute f = \case
  JsonNull              -> pure JsonNull
  JsonTrue              -> pure JsonTrue
  JsonFalse             -> pure JsonFalse
  JsonText x            -> pure (JsonText x)
  JsonInteger x         -> pure (JsonInteger x)
  JsonDouble x          -> pure (JsonDouble x)
  JsonObject (Object o) -> JsonObject . Object <$> traverse (substitute f) o
  JsonArray xs          -> JsonArray <$> traverse (substitute f) xs
  JsonCombine xs        -> JsonCombine <$> traverse (substitute f) xs
  JsonCustom nm x       -> f nm x

ppValue :: (a -> PP.Doc) -> Value f a -> PP.Doc
ppValue f = \case
  JsonNull        -> PP.text "null"
  JsonTrue        -> PP.text "true"
  JsonFalse       -> PP.text "false"
  JsonText x      -> PP.text (show x)
  JsonInteger n   -> PP.text (show n)
  JsonDouble n    -> PP.text (show n)
  JsonObject o    -> ppObject f o
  JsonArray xs    -> ppArray f xs
  JsonCombine _xs -> error "TODO"
  JsonCustom nm x -> PP.braces $ (PP.colon <> PP.text (Text.unpack nm)) PP.<+> f x

ppArray :: (a -> PP.Doc) -> [Value f a] -> PP.Doc
ppArray f xs =
  PP.brackets (PP.nest 2 (foldr (.) id (addCommas items) mempty))
  where
    items     = map go xs
    go x      = (ppValue f x PP.$$)
    addCommas = intersperse (<> PP.comma)


-- JSON OBJECT

getObject :: Object f a -> Map Text (Value f a)
getObject (Object x) = x

ppObject :: (a -> PP.Doc) -> Object f a -> PP.Doc
ppObject f (Object m) =
  PP.braces (PP.nest 2 (foldr (.) id (addCommas fields) mempty))
  where
    fields    = map (uncurry go) (Map.toList m)
    go k v    = (PP.$$ ((PP.text (show k) <> PP.colon) PP.<+> ppValue f v))
    addCommas = intersperse (<> PP.comma)


-- PARSER

data Parser m e f x a
  = Parser
      (Map Char [CustomParser m e x -> Combine f -> P.ParsecT e Text m a])
      (CustomParser m e x -> Combine f -> P.ParsecT e Text m a)
    deriving Functor

instance Ord e => Semigroup (Parser m e f x a) where
  Parser a x <> Parser b y = Parser (Map.unionWith (++) a b) (\c d -> x c d <|> y c d)

instance Ord e => Monoid (Parser m e f x a) where
  mempty = Parser Map.empty (\_ _ -> empty)
  mappend = (<>)

runParser :: Ord e => Combine f -> Parser m e f x a -> P.ParsecT e Text m a
runParser = runParser' emptyCustom

runParser'
  :: Ord e
  => CustomParser m e x
  -> Combine f
  -> Parser m e f x a -> P.ParsecT e Text m a
runParser' c d (Parser ms p) =
  (lexeme (P.oneOf (Map.keys ms)) >>= \x -> asum (map (\f -> f c d) (ms Map.! x))) <|> p c d

prefixParser
  :: Ord e
  => Char
  -> (CustomParser m e x -> Combine f -> P.ParsecT e Text m a)
  -> Parser m e f x a
prefixParser c m = Parser (Map.singleton c [m]) (\_ _ -> empty)

otherParser
  :: (CustomParser m e x -> Combine f -> P.ParsecT e Text m a)
  -> Parser m e f x a
otherParser = Parser Map.empty


-- CUSTOM VALUES

newtype CustomParser m e x = CustomParser (P.ParsecT e Text m (Text, x))

emptyCustom :: Ord e => CustomParser m e x
emptyCustom = customParsers []

customParsers :: Ord e => [(Text, P.ParsecT e Text m x)] -> CustomParser m e x
customParsers ps = CustomParser $
  asum [(nm,) <$> (symbol (":" <> nm) *> psr) | (nm, psr) <- ps]

withCustom :: CustomParser m e x -> Parser m e f x a -> Parser m e f x a
withCustom c (Parser x y) = Parser (fmap (map (\k _ -> k c)) x) (\_ -> y c)


-- COMBINERS

-- A combiner needs a way to inject a value into a structure type and a way to
-- see if there is a single value contained in the structure.
data Combine f = Combine (forall a. a -> f a) (forall a. f a -> Maybe a)

combineList :: Combine NonEmpty
combineList = Combine (:|[]) (\(x:|xs) -> if null xs then Just x else Nothing)

combineVoidF :: Combine VoidF
combineVoidF = Combine (error "combineVoidF: ka-boom!") (const Nothing)

injectVoidF :: Value VoidF a -> Value NonEmpty a
injectVoidF = \case
  JsonNull              -> JsonNull
  JsonTrue              -> JsonTrue
  JsonFalse             -> JsonFalse
  JsonText x            -> JsonText x
  JsonInteger n         -> JsonInteger n
  JsonDouble n          -> JsonDouble n
  JsonObject (Object o) -> JsonObject (Object (fmap injectVoidF o))
  JsonArray xs          -> JsonArray (map injectVoidF xs)
  JsonCombine xs        -> absurdF xs
  JsonCustom nm x       -> JsonCustom nm x

elimCombine
  :: (Monad m, Traversable f)
  => (Value VoidF a -> Value VoidF a -> m (Value VoidF a))
  -> Value f a -> m (Value VoidF a)
elimCombine f = \case
  JsonNull              -> pure JsonNull
  JsonTrue              -> pure JsonTrue
  JsonFalse             -> pure JsonFalse
  JsonText x            -> pure (JsonText x)
  JsonInteger x         -> pure (JsonInteger x)
  JsonDouble x          -> pure (JsonDouble x)
  JsonObject (Object o) -> JsonObject . Object <$> traverse (elimCombine f) o
  JsonArray xs          -> JsonArray <$> traverse (elimCombine f) xs
  JsonCustom nm x       -> pure (JsonCustom nm x)
  JsonCombine xs -> do
    ys <- traverse (elimCombine f) xs
    case toList ys of
      a:as -> foldM f a as
      []   -> error "impossible: JsonCombine must be VoidF or NonEmpty"


-- VALUE PARSERS

value
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x (Value f x)
value = mconcat [atom, text, number, array, combine (object <> custom)]

object
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x (Value f x)
object = JsonObject <$> object'

array
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x (Value f x)
array = JsonArray <$> array'

text :: Ord e => Parser m e f x (Value f x)
text = JsonText <$> text'

number :: Ord e => Parser m e f x (Value f x)
number = either JsonInteger JsonDouble <$> number'

atom :: Ord e => Parser m e f x (Value f x)
atom = mconcat
  [ otherParser (\_ _ -> (JsonNull  <$ symbol "null"))
  , otherParser (\_ _ -> (JsonTrue  <$ symbol "true"))
  , otherParser (\_ _ -> (JsonFalse <$ symbol "false"))
  ]

combine
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x (Value f x)
  -> Parser m e f x (Value f x)
combine ps = otherParser $ \c d ->
  runParser' c d (combine' ps) >>= either pure (pure . JsonCombine)

custom :: Ord e => Parser m e f x (Value f x)
custom = uncurry JsonCustom <$> custom'


-- TYPEFUL PARSERS

object'
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x (Object f x)
object' = prefixParser '{' $ \c d -> do
  pairs <- flip P.sepEndBy (symbol ",") $
    (,) <$> (lexeme (runParser' c d text') <* symbol ":")
        <*> lexeme (runParser' c d value)
  let repeats = map fst $
        let ks = sort (map fst pairs)
        in filter (uncurry (==)) (zip ks (drop 1 ks))
  if null repeats
    then Object (Map.fromList pairs) <$ symbol "}"
    else fail ("Repeated object keys: " ++ show repeats)

array'
  :: (Ord e, Semigroup (f (Value f x)))
  => Parser m e f x [Value f x]
array' = prefixParser '[' $ \c d ->
  P.sepBy (lexeme (runParser' c d value)) (symbol ",") <* symbol "]"

text' :: Ord e => Parser m e f x Text
text' = otherParser $ \_ _ -> P.label "string literal" $ P.C.char '"' *> go id
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
integer' = otherParser $ \_ _ -> P.label "integer" $ do
  (neg, n) <- pInteger
  pure $ (if neg then negate else id) n

number' :: Ord e => Parser m e f x (Either Int Double)
number' = otherParser $ \_ _ -> P.label "number" $ do
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

combine'
  :: (Ord e, Semigroup (f a))
  => Parser m e f x a -> Parser m e f x (Either a (f a))
combine' p = otherParser $ \c d -> do
  let Combine inject inspect = d
  let psr     = runParser' c d p
  let psrMany = (symbol "<>" *> P.sepBy psr (symbol "<>")) <|> pure []
  x <- psr
  fromMaybe (pure []) (inspect (inject psrMany)) >>= \case
    [] -> pure (Left x)
    xs -> pure (Right (sconcat (fmap inject (x :| xs))))

custom' :: Ord e => Parser m e f x (Text, x)
custom' = prefixParser '{' $ \(CustomParser c) _ -> c <* symbol "}"


-- Helpers

parseHex :: String -> Maybe Int
parseHex = foldl' (liftA2 (\i x -> i * 16 + x)) (Just 0) . map f
  where
    f x | isDigit x            = Just (ord x - 48)
        | isUpper x && x < 'G' = Just (ord x - 55)
        | isLower x && x < 'g' = Just (ord x - 87)
        | otherwise            = Nothing
