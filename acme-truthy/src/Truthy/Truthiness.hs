module Truthy.Truthiness
  ( Truthiness (..)
  ) where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Read (Read)
import Text.Show (Show)

data Truthiness
  = Truthy -- ^ https://developer.mozilla.org/en-US/docs/Glossary/Truthy
  | Falsy  -- ^ https://developer.mozilla.org/en-US/docs/Glossary/Falsy
  deriving (Eq, Ord, Read, Show)
