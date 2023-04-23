module Parser.Common where

import Control.Applicative
import Data.Char
import Data.Maybe
import Serializer

newtype CssStringLiteral = CssStringLiteral String deriving (Show, Eq)

instance Serializable CssStringLiteral where
    serialize (CssStringLiteral a) = a

newtype CssNumberLiteral = CssNumberLiteral (Float, Maybe CssUnit) deriving (Show, Eq)

instance Serializable CssNumberLiteral where
    serialize (CssNumberLiteral (value, Just (CssUnit unit))) = show value <> unit
    serialize (CssNumberLiteral (value, _)) = show value

newtype CssUnit = CssUnit String deriving (Show, Eq)

instance Serializable CssUnit where
    serialize (CssUnit a) = a

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

-- Combine two parsers p1 and p2 to create a new parser p3
-- p2 is optional
optionalP :: Parser a -> Parser b -> Parser (a, Maybe b)
optionalP (Parser p1) (Parser p2) = Parser $ \input -> do
    (input', a) <- p1 input
    case p2 input' of
        Just (input'', b) -> Just (input'', (a, Just b))
        Nothing -> Just (input', (a, Nothing))

-- Given a list of parsers p1, p2, ... pn. Try each one until one succeed,
-- start with the left most one
alternatePs :: [Parser a] -> Parser a
alternatePs = asum

alternateP2 :: Parser a -> Parser b -> Parser (a, b)
alternateP2 = liftA2 (,)

-- Allow parsing pattern like this ABBAABBBA
-- We want multiple A while they are intersperse by multiple B
intersperseBy :: Parser a -> Parser b -> Parser [a]
intersperseBy (Parser p1) (Parser p2) = fromMaybe [] . mconcat <$> many f
  where
    f = Parser $ \input -> case p1 input of
        Just (input', x) -> Just (input', Just [x])
        Nothing -> case p2 input of
            Just (input'', y) -> Just (input'', Nothing)
            Nothing -> Nothing

-- Primitive parsers

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
        | y == x = Just (ys, y)
        | otherwise = Nothing
    f [] = Nothing

charPSelective :: (Char -> Bool) -> Parser Char
charPSelective predicate = Parser g
  where
    g (y : ys)
        | predicate y = Just (ys, y)
        | otherwise = Nothing
    g [] = Nothing

stringP :: String -> Parser String
stringP = traverse charP

stringPSelective :: (Char -> Bool) -> Parser String
stringPSelective predicate = many $ charPSelective predicate

notNullP :: Parser [a] -> Parser [a]
notNullP (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

whiteSpacesP :: Parser String
whiteSpacesP = many single
  where
    single = alternatePs $ charP . chr <$> codepoints
    codepoints = [9, 32]

-- Same as whiteSpacesP except newline is allowed
lWhiteSpacesP :: Parser String
lWhiteSpacesP = many single
  where
    single = alternatePs $ charP . chr <$> codepoints
    codepoints = [9, 10, 32]

withTrailingWsps :: Parser a -> Parser a
withTrailingWsps p = p <* whiteSpacesP

withLeadingWsps :: Parser a -> Parser a
withLeadingWsps p = whiteSpacesP *> p

-- CSS specific parsers
stringLiteralP :: Parser CssStringLiteral
stringLiteralP = CssStringLiteral <$> some (charPSelective isLetter <|> charP '-')

numberLiteralP :: Parser CssNumberLiteral
numberLiteralP = CssNumberLiteral <$> valueP `optionalP` unitP
  where
    valueP = toFloat <$> notNullP (stringPSelective isDigit) `optionalP` fractionalP
    fractionalP = liftA2 (:) (charP '.') (notNullP (stringPSelective isDigit))
    toFloat (integerPart, fractionalPart) = read $ integerPart <> fromMaybe "" fractionalPart

-- Let's add them later
cssUnits = ["px"]

unitP :: Parser CssUnit
unitP = CssUnit <$> alternatePs ps
  where
    ps = map stringP cssUnits
