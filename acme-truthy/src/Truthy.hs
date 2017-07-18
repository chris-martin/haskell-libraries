{- |

>>> truthy "hello"
Truthy

>>> truthy ""
Falsy

>>> "hello" && "world"
Truthy

>>> "hello" && 0
Falsy

>>> if' 4 "yes" "no"
"yes"

>>> if' 0 "yes" "no"
"no"

-}
module Truthy
  ( Truthiness
  , Truthy (..)
  , Not (..)

  -- * Operations
  , if'
  , (&&)
  , (||)
  , not

  ) where

import Truthy.Class
import Truthy.Not
import Truthy.Truthiness

if' :: Truthy a => a -> b -> b -> b
if' condition whenTruthy whenFalsy =
  case truthy condition of
    Truthy -> whenTruthy
    Falsy  -> whenFalsy

(&&) :: (Truthy a, Truthy b) => a -> b -> Truthiness
a && b = if' a (truthy b) Falsy

(||) :: (Truthy a, Truthy b) => a -> b -> Truthiness
a || b = if' a Truthy (truthy b)
