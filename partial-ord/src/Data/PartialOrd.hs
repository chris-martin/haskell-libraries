{-# LANGUAGE DefaultSignatures #-}

module Data.PartialOrd

  ( PartialOrdering (..)
  , PartialOrd (..)

  -- * Tools for defining 'PartialOrd' instances
  , totalCompare'

  -- * Newtypes with 'PartialOrd' instances
  , Dual (..)

  ) where

import Data.Bool
import Data.Eq
import Data.Ord (Ord (compare), Ordering (..))
import Data.Set (Set)
import Text.Show

import qualified Data.Set as Set

data PartialOrdering = NoRelation | Relation Ordering
  deriving (Eq, Show)

class Eq a => PartialOrd a
  where
    {-# MINIMAL compare' | ((<=), related) #-}

    compare' :: a -> a -> PartialOrdering
    compare' a b =
      case related a b of
        False         -> NoRelation
        True | a == b -> Relation EQ
        True | a <= b -> Relation LT
        True          -> Relation GT

    (<), (<=), (>), (>=), related :: a -> a -> Bool

    related a b =
      case compare' a b of
        Relation _ -> True
        _ -> False

    a < b =
      case compare' a b of
        Relation LT -> True
        _ -> False

    a > b =
      case compare' a b of
        Relation GT -> True
        _ -> False

    a >= b =
      case compare' a b of
        Relation GT -> True
        Relation EQ -> True
        _ -> False

    a <= b =
      case compare' a b of
        Relation LT -> True
        Relation EQ -> True
        _ -> False

totalCompare' :: Ord a => a -> a -> PartialOrdering
totalCompare' a b = Relation (compare a b)

{- |

>>> compare' (Set.fromList [1,2,3]) (Set.fromList [1,2,3])
Relation EQ

>>> compare' (Set.fromList [1,2,3]) (Set.fromList [1,3])
Relation GT

>>> compare' (Set.fromList [1,2]) (Set.fromList [2,3])
NoRelation

>>> Set.fromList [1,2,3] >= Set.fromList [1,3]
True

>>> Set.fromList [1,2] < Set.fromList [2,3]
False

>>> Set.fromList [1,2] >= Set.fromList [2,3]
False

-}
instance Ord a => PartialOrd (Set a)
  where

    a <= b = Set.isSubsetOf a b
    a >= b = Set.isSubsetOf b a

    a < b = Set.isProperSubsetOf a b
    a > b = Set.isProperSubsetOf b a

    related a b =
      Set.isSubsetOf a b ||
      Set.isSubsetOf b a

newtype Dual a = Dual a
  deriving Eq

{- |

>>> compare' (Dual (Set.fromList [1,2,3])) (Dual (Set.fromList [1,2,3]))
Relation EQ

>>> compare' (Dual (Set.fromList [1,2,3])) (Dual (Set.fromList [1,3]))
Relation LT

>>> compare' (Dual (Set.fromList [1,2])) (Dual (Set.fromList [2,3]))
NoRelation

>>> Dual (Set.fromList [1,2,3]) >= Dual (Set.fromList [1,3])
False

>>> Dual (Set.fromList [1,2]) < Dual (Set.fromList [2,3])
False

>>> Dual (Set.fromList [1,2]) >= Dual (Set.fromList [2,3])
False

-}
instance (Eq a, PartialOrd a) => PartialOrd (Dual a)
  where

    related (Dual a) (Dual b) =
      related a b

    Dual a <= Dual b =
      b <= a
