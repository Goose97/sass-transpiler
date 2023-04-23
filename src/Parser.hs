module Parser (
    parse,
    SheetAst,
)
where

import Control.Applicative
import Data.List
import Parser.Block
import Parser.Common
import Serializer

newtype SheetAst = SheetAst [Block] deriving (Show, Eq)

instance Serializable SheetAst where
    serialize (SheetAst blocks) = mconcat . intersperse "\n\n" . fmap serialize $ blocks

parse :: String -> Either String SheetAst
parse x = case runParser (blockP `intersperseBy` notNullP lWhiteSpacesP) x of
    Just ("", blocks) -> Right (SheetAst blocks)
    Just (remain, _) -> Left "Parse error"
    Nothing -> Left "Parse error"
