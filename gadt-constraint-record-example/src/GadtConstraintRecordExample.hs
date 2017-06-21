{-# LANGUAGE DeriveGeneric, FlexibleContexts, GADTs, StandaloneDeriving,
             TypeFamilies #-}

module GadtConstraintRecordExample (Foo (..), Bar (..)) where

import Prelude (Show)

class (Show (X a), Show (Y a)) => Foo a
  where
    type X a
    type Y a

data Bar a
  where
    Bar :: Foo a =>
      { x :: X a
      , y :: Y a
      } -> Bar a

deriving instance Show (Bar a)
