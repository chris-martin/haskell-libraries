{- |

>>> 1 :: PowerOfTwo
PowerOfTwo {logBase2 = 0}

>>> 2 :: PowerOfTwo
PowerOfTwo {logBase2 = 1}

>>> 4 :: PowerOfTwo
PowerOfTwo {logBase2 = 2}

>>> 1024 :: PowerOfTwo
PowerOfTwo {logBase2 = 10}

>>> fromInteger (2^1234567) :: PowerOfTwo
PowerOfTwo {logBase2 = 1234567}

>>> PowerOfTwo 5 + PowerOfTwo 5
PowerOfTwo {logBase2 = 6}

>>> PowerOfTwo 3 * PowerOfTwo 7
PowerOfTwo {logBase2 = 10}

-}

module Numeric.PowerOfTwo (PowerOfTwo (..)) where

import Math.NumberTheory.Logarithms (integerLog2')
import Numeric.Natural
import Prelude
  (Eq (..), Num (..), Ord (..), Read, Show, (^), error, fromIntegral)

newtype PowerOfTwo = PowerOfTwo { logBase2 :: Natural }
  deriving (Eq, Ord, Read, Show)

instance Num PowerOfTwo
  where

    PowerOfTwo a + PowerOfTwo b =
      if a == b
        then PowerOfTwo (a + 1)
        else error "PowerOfTwo: result of (+) is not a power of two"

    PowerOfTwo a * PowerOfTwo b =
      PowerOfTwo (a + b)

    abs x = x

    signum _ = PowerOfTwo 0

    fromInteger n =
      if n < 1
        then error "PowerOfTwo: fromInteger used with a negative argument"
        else
          let
            e = integerLog2' n
          in
            if 2 ^ e == n
              then PowerOfTwo (fromIntegral e)
              else error "PowerOfTwo: fromInteger arg is not a power of two"

    negate _ =
      error "PowerOfTwo: cannot negate"
