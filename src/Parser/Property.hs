module Parser.Property where

import Control.Applicative
import Parser.Common
import Serializer

data PropertyValue
    = PropertyStringValue CssStringLiteral
    | PropertyNumberValue CssNumberLiteral
    deriving (Show, Eq)

data Property = Property
    { name :: CssStringLiteral
    , value :: [PropertyValue]
    }
    deriving (Show, Eq)

instance Serializable Property where
    serialize Property{name = name, value = value} = serialize name <> ": " <> s <> ";"
      where
        s = mconcat (f <$> value)
        f (PropertyStringValue a) = serialize a
        f (PropertyNumberValue a) = serialize a

-- Let's add them later
propertyP :: Parser Property
propertyP = liftA3 f name colon value <* charP ';'
  where
    name = withTrailingWsps stringLiteralP
    colon = withTrailingWsps (charP ':')
    value = some $ withTrailingWsps propertyValueP
    f name _ value = Property{name = name, value = value}

propertyValueP :: Parser PropertyValue
propertyValueP = f <|> g
  where
    f = PropertyStringValue <$> stringLiteralP
    g = PropertyNumberValue <$> numberLiteralP
