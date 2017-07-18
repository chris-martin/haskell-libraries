module Truthy.Class
  ( Truthy (..)
  ) where

import Truthy.Truthiness

import Data.Bool (Bool (..))
import Data.Eq (Eq (..))
import Data.Foldable (Foldable (..))
import Data.Function ((.))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Monoid (Monoid (..))
import Prelude (Float, Double, Integer, Num (..))

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Map
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Tuple.Only
import qualified Data.Vector

class Truthy a
  where

    -- | Coerce a value to a 'Bool'.
    truthy :: a -> Truthiness

falsyMempty :: (Eq a, Monoid a) => a -> Truthiness
falsyMempty x = if x == mempty then Falsy else Truthy

falsyNull :: Foldable f => f a -> Truthiness
falsyNull x = if null x then Falsy else Truthy

falsyZero :: (Eq a, Num a) => a -> Truthiness
falsyZero x = if x == 0 then Falsy else Truthy

instance Truthy Bool
  where
    truthy True = Truthy
    truthy False = Falsy

instance Truthy a => Truthy (Identity a)
  where
    truthy = truthy . runIdentity

instance Truthy a => Truthy (Data.Tuple.Only.Only a)
  where
    truthy = truthy . Data.Tuple.Only.fromOnly

instance Truthy [a]
  where
    truthy = falsyNull

instance Truthy (Data.Vector.Vector a)
  where
    truthy = falsyNull

instance Truthy (Data.Map.Map k a)
  where
    truthy = falsyNull

instance Truthy (Data.Set.Set a)
  where
    truthy = falsyNull

instance Truthy (Data.Sequence.Seq a)
  where
    truthy = falsyNull

instance Truthy Data.ByteString.ByteString
  where
    truthy = falsyMempty

instance Truthy Data.ByteString.Lazy.ByteString
  where
    truthy = falsyMempty

instance Truthy Data.Text.Text
  where
    truthy = falsyMempty

instance Truthy Data.Text.Lazy.Text
  where
    truthy = falsyMempty

instance Truthy Data.Text.Lazy.Builder.Builder
  where
    truthy = falsyMempty

instance Truthy Float
  where
    truthy = falsyZero

instance Truthy Double
  where
    truthy = falsyZero

instance Truthy Integer
  where
    truthy = falsyZero

instance Truthy Int
  where
    truthy = falsyZero

instance Truthy Int8
  where
    truthy = falsyZero

instance Truthy Int16
  where
    truthy = falsyZero

instance Truthy Int32
  where
    truthy = falsyZero

instance Truthy Int64
  where
    truthy = falsyZero
