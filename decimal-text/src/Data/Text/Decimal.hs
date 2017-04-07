module Data.Text.Decimal
    ( DecimalText (..)
    ) where

import Data.Text (Text)

import qualified Data.Text.Lazy as L

-- | A class for things which have decimal representations. These functions
-- must produce text containing only the characters
--
-- * @0@-@9@
-- * @-@ indicating negation
-- * @.@, the decimal point
class DecimalText a where

    decimalText :: a -> Text
    decimalText a = L.toStrict (decimalText' a)

    -- | Lazy version of 'decimalText'. This is provided to allow for repeating
    -- decimals whose representations are infinitely long.
    decimalText' :: a -> L.Text
    decimalText' a = L.fromStrict (decimalText a)

    {-# MINIMAL decimalText | decimalText' #-}
