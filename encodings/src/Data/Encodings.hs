module Data.Encodings
    ( Encoding (..)
    ) where

-- | 'Encoding' is similar to @Prism@ from the lens library, but with no laws.
data Encoding a b f = Encoding
    { encode :: a -> b
    , decode :: b -> f a
    }
