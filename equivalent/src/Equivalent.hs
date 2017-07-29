module Equivalent
  ( equivalent
  ) where

import Data.List (partition)
import Prelude

{- |
>>> import Data.Function (on)
>>> equivalent ((==) `on` (`mod` 3)) [1..10]
[[1,4,7,10],[2,5,8],[3,6,9]]
-}
equivalent :: (a -> a -> Bool) -> [a] -> [[a]]
equivalent _ [] = []
equivalent eq (x:xs) =
  let (group, others) = partition (eq x) xs
  in  (x:group) : equivalent eq others
