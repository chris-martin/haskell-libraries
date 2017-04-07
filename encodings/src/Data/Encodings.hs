module Data.Encodings
    (
    -- * Types
      Encoding, Iso

    -- * Constructing encodings
    , encoding, iso

    -- * Using encodings
    , encode, decode

    ) where

import Data.Functor (fmap)
import Data.Functor.Identity

-- | 'Encoding' is similar to @Prism@ from the lens library, but with no laws.
data Encoding a b f = Encoding
    { encode :: a -> b
    , decode :: b -> f a
    }

encoding :: (a -> b) -> (b -> f a) -> Encoding a b f
encoding = Encoding

type Iso a b = Encoding a b Identity

iso :: (a -> b) -> (b -> a) -> Iso a b
iso f g = Encoding f (fmap Identity g)
