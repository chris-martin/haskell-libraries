module Truthy.Not
  ( Not (..)
  , not
  ) where

import Truthy.Class
import Truthy.Truthiness

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Read (Read)
import Text.Show (Show)

newtype Not a = Not a
  deriving (Eq, Ord, Read, Show)

instance Truthy a => Truthy (Not a)
  where
    truthy = not

not :: Truthy a => a -> Truthiness
not x =
  case truthy x of
    Truthy -> Falsy
    Falsy -> Truthy
