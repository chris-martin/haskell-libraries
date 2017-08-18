{-# LANGUAGE LambdaCase #-}

module Puzzle.BadTwentyFour where

import Data.IntMultiSet (IntMultiSet)
import Data.Map (Map)
import Prelude
import Text.Show (Show (..), showParen, showString)

import qualified Data.Map as Map
import qualified Data.IntMultiSet as S
import qualified Data.Ratio as Ratio

type S = IntMultiSet

data Expr =
  Add Expr Expr |
  Sub Expr Expr |
  Mul Expr Expr |
  Div Expr Expr |
  Const Int

instance Num Expr
  where

    (+) = Add
    (-) = Sub
    (*) = Mul

    fromInteger = Const . fromInteger

    signum = error "Expr does not support signum"
    abs    = error "Expr does not support abs"

instance Fractional Expr
  where

    (/) = Div

    fromRational x = Div (fromInteger $ Ratio.numerator x)
                         (fromInteger $ Ratio.denominator x)

instance Show Expr
  where
    showsPrec = exprShowsPrec

exprShowsPrec :: Int -> Expr -> ShowS
exprShowsPrec _ =
  \case
    Add x y -> exprShows '+' x y
    Sub x y -> exprShows '-' x y
    Mul x y -> exprShows '*' x y
    Div x y -> exprShows '/' x y
    Const x -> shows x

exprShows :: Char -> Expr -> Expr -> ShowS
exprShows op x y =
  showParen True $
  shows x .
  showString [' ', op, ' '] .
  shows y

eval :: Expr -> Rational
eval =
  \case
    Add x y -> eval x + eval y
    Sub x y -> eval x - eval y
    Mul x y -> eval x * eval y
    Div x y -> eval x / eval y
    Const x -> fromIntegral x

goodExpr :: Expr -> Bool
goodExpr e = eval e == 24

{- |
>>> fmap eval uniformExprs
[24 % 1,24 % 1,24 % 1,24 % 1,24 % 1,24 % 1,24 % 1,24 % 1,24 % 1]
-}
uniformExprs :: [Expr]
uniformExprs =
  [ (1 + 1) * (1 + 1) * (1 + 1) * (1 + 1 + 1)
  , 2 * 2 * 2 + 2 * 2 * 2 * 2
  , 3 * 3 * 3 - 3
  , 4 * 4 + 4 + 4
  , 5 * 5 - 5 / 5
  , 6 + 6 + 6 + 6
  , (7 + 7 + 7) + (7 + 7 + 7) / 7
  , 8 + 8 + 8
  , 9 + 9 + 9 - (9 + 9 + 9) / 9
  ]
