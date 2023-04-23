module Parser.Selector where

import Control.Applicative
import Data.Functor
import Data.List
import Parser.Common
import Serializer

data SimpleSelector
    = IdSelector CssStringLiteral
    | ClassSelector CssStringLiteral
    | TagnameSelector CssStringLiteral
    deriving (Show, Eq)

instance Serializable SimpleSelector where
    serialize (IdSelector a) = "#" <> serialize a
    serialize (ClassSelector a) = "." <> serialize a
    serialize (TagnameSelector a) = serialize a

data AttributeSelector = AttributeSelector
    { name :: CssStringLiteral
    , value :: Maybe String
    , comparator :: Maybe String
    }
    deriving (Show, Eq)

instance Serializable AttributeSelector where
    serialize
        AttributeSelector
            { name = name
            , value = value
            , comparator = comparator
            } = "[" <> serialize name <> serialize comparator <> f value <> "]"
          where
            f (Just x) = show x
            f _ = ""

data ExtendedSelector = ExtendedSelector
    { selectors :: [SimpleSelector]
    , pseudoClass :: Maybe String
    , attribute :: Maybe AttributeSelector
    }
    deriving (Show, Eq)

instance Serializable ExtendedSelector where
    serialize
        ExtendedSelector
            { selectors = selectors
            , pseudoClass = pseudoClass
            , attribute = attribute
            } = f <> g <> h
          where
            f = mconcat $ serialize <$> selectors
            g = serialize $ (":" <>) <$> pseudoClass
            h = serialize attribute

data Combinator
    = DescendantCombinator
    | ChildCombinator
    | AdjacentSiblingCombinator
    | GeneralSiblingCombinator
    deriving (Show, Eq)

instance Serializable Combinator where
    serialize DescendantCombinator = " "
    serialize ChildCombinator = ">"
    serialize AdjacentSiblingCombinator = "+"
    serialize GeneralSiblingCombinator = "~"

-- a > b + c == CompoundSelector (a, [(b, >), (c, +)])
newtype CompoundSelector = CompoundSelector (ExtendedSelector, [(ExtendedSelector, Combinator)]) deriving (Show, Eq)

instance Serializable CompoundSelector where
    serialize (CompoundSelector (base, others)) = mconcat $ serialize base : concatMap f others
      where
        f (selector, combinator) = [serializeCombinator combinator, serialize selector]
        serializeCombinator DescendantCombinator = " "
        serializeCombinator x = " " <> serialize x <> " "

simpleSelectorP :: Parser SimpleSelector
simpleSelectorP = f <|> g <|> h
  where
    f = IdSelector <$> (charP '#' *> stringLiteralP)
    g = ClassSelector <$> (charP '.' *> stringLiteralP)
    h = TagnameSelector <$> stringLiteralP

attributeSelectorP :: Parser AttributeSelector
attributeSelectorP = toSelector <$> (charP '[' *> (stringLiteralP `optionalP` compareExprP) <* charP ']')
  where
    compareExprP = liftA2 (,) attrComparatorP (charP '"' *> betweenQuote <* charP '"')
    betweenQuote = some $ charPSelective (/= '"')
    toSelector (attributeName, Just (comparator, expr)) = AttributeSelector{name = attributeName, value = Just expr, comparator = Just comparator}
    toSelector (attributeName, _) = AttributeSelector{name = attributeName, value = Nothing, comparator = Nothing}

comparators = ["=", "~=", "*=", "$="]

attrComparatorP :: Parser String
attrComparatorP = alternatePs $ stringP <$> comparators

pseudoClasses = ["first-child", "last-child", "only-child", "invalid", "hover", "focus"]

pseudoClassP :: Parser String
pseudoClassP = charP ':' *> alternatePs (map stringP pseudoClasses)

extendedSelectorP :: Parser ExtendedSelector
extendedSelectorP = toSelector . flatten <$> (some simpleSelectorP `optionalP` pseudoClassP `optionalP` attributeSelectorP)
  where
    toSelector (x, y, z) = ExtendedSelector{selectors = x, pseudoClass = y, attribute = z}

combinators = ['>', '+', '~']

combinatorP :: Parser Combinator
combinatorP = alternatePs $ toParser <$> combinators
  where
    toParser = fmap f . charP
    f '>' = ChildCombinator
    f '+' = AdjacentSiblingCombinator
    f '~' = GeneralSiblingCombinator

selectorP :: Parser CompoundSelector
selectorP = toSelector <$> liftA2 (,) extendedSelectorP (many withCombinator)
  where
    withCombinator = liftA2 (flip (,)) combinatorAndWsps extendedSelectorP
    -- It sucks that we have to handle descendant combinator separately
    combinatorAndWsps =
        (withLeadingWsps . withTrailingWsps $ combinatorP)
            <|> withTrailingWsps (charP ' ' $> DescendantCombinator)
    toSelector (base, others) = CompoundSelector (base, others)

flatten :: ((a, b), c) -> (a, b, c)
flatten x = (fst . fst $ x, snd . fst $ x, snd x)

appendSelector :: CompoundSelector -> CompoundSelector -> Combinator -> CompoundSelector
appendSelector
    (CompoundSelector (appendBase, appendOthers))
    (CompoundSelector (base, others))
    combinator =
        CompoundSelector (appendBase, appendOthers ++ ((base, combinator) : others))
