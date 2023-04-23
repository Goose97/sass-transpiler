{-# LANGUAGE FlexibleInstances #-}

module Serializer where

class Serializable a where
    serialize :: a -> String

instance (Serializable a) => Serializable (Maybe a) where
    serialize (Just a) = serialize a
    serialize _ = ""

instance Serializable String where
    serialize = id
