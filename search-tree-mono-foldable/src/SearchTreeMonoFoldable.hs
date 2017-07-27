{-# LANGUAGE DeriveFoldable, TypeFamilies #-}

module SearchTreeMonoFoldable
  ( Tree (..)
  ) where

import Data.MonoTraversable
import Prelude

data Tree a = Nil | Node (Tree a) a (Tree a)
  deriving Foldable

type instance Element (Tree a) = a

instance Ord a => MonoFoldable (Tree a) where

  ofoldr _ z Nil = z
  ofoldr f z (Node l d r) = ofoldr f (f d (ofoldr f z l)) r

  oelem x Nil = False
  oelem x (Node l d r)
    | x == d = True
    | x < d = elem x l
    | otherwise = elem x r
