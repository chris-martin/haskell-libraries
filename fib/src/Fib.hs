{-# LANGUAGE DataKinds, DeriveFunctor, TypeFamilies, TypeOperators #-}

module Fib
  ( fibs
  ) where

import Control.Applicative (Applicative (..))
import Control.Arrow ((>>>))
import Data.Functor (Functor (..), (<$>))
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Numeric.Natural
import Prelude (Num (..), undefined)

fibs :: [Natural]
fibs = toList fibs'

fibs' :: Many Natural
fibs' = 0 :+ 1 :+ repeat ((+) <$> back 1 <*> back 2)

data Many a = Nil | One a :+ Many a

infixr 5 :+

--toList :: Many a -> [a]
--toList Nil = []
--toList (Fixed a :+ xs) = a : toList xs
--toList (B

data One a = Fixed a | Back Natural
  deriving Functor

instance Applicative One
  where
    pure = Fixed
    af <*> ax = undefined

instance Num a => Num (One a)
  where
    fromInteger = fromInteger >>> Fixed

back :: Natural -> One a
back n = undefined

repeat :: One a -> Many a
repeat = undefined
