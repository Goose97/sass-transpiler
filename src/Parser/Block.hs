module Parser.Block where

import Control.Applicative
import Data.List
import Parser.Common
import Parser.Property
import Parser.Selector
import Serializer

data Block = Block
    { selector :: CompoundSelector
    , body :: [BlockItem]
    }
    deriving (Show, Eq)

instance Serializable Block where
    serialize (Block{selector = selector, body = body}) =
        sSelector
            <> " {\n"
            <> (mconcat . intersperse "\n" . fmap f . filter isItemProperty) body
            <> " }"
            <> nestedBlocks
      where
        sSelector = serialize selector
        f = ("  " <>) . serialize
        nestedBlocks = mconcat $ filter isItemBlock body >>= g
        -- Append the current selector to the nested block
        -- For now, let's assume it is descendant selector
        g x = ["\n\n", serialize . withContextSelector $ x]
        withContextSelector (ItemBlock b) = b{selector = appended}
          where
            appended = appendSelector selector (Parser.Block.selector b) DescendantCombinator

data BlockItem
    = ItemProperty Property
    | ItemBlock Block
    deriving (Show, Eq)

instance Serializable BlockItem where
    serialize (ItemProperty p) = serialize p
    serialize (ItemBlock b) = serialize b

isItemProperty :: BlockItem -> Bool
isItemProperty (ItemProperty _) = True
isItemProperty (ItemBlock _) = False

isItemBlock :: BlockItem -> Bool
isItemBlock (ItemProperty _) = False
isItemBlock (ItemBlock _) = True

blockP :: Parser Block
blockP = liftA2 toBlock selectorP (open *> blockItemsP <* close)
  where
    toBlock selector items = Block{selector = selector, body = items}
    open = whiteSpacesP *> charP '{' *> lWhiteSpacesP
    close = lWhiteSpacesP *> charP '}' *> whiteSpacesP

blockItemsP :: Parser [BlockItem]
blockItemsP = p `intersperseBy` notNullP lWhiteSpacesP
  where
    p = (ItemProperty <$> propertyP) <|> (ItemBlock <$> blockP)
