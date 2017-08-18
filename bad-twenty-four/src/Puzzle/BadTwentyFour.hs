{-# LANGUAGE LambdaCase #-}

module Puzzle.BadTwentyFour where

import Data.IntMultiSet (IntMultiSet)
import Prelude
import Text.Show (Show (..), showParen, showString)

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
