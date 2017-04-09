module IRS.Prelude
    ( non_negative, or_zero

    -- * Re-exports
    , Eq, Show, Num (..), Integer, (%), Maybe (..), min, max, round

    ) where

import Data.Ratio
import Prelude

-- | If the number is negative, make it 0.
non_negative :: Integer -> Integer
non_negative = max 0

-- | Convert @Just x@ to @x@ and @Nothing@ to @0@.
or_zero :: Maybe Integer -> Integer
or_zero (Just x) = x
or_zero Nothing = 0
