{-# LANGUAGE LambdaCase #-}

{- |

>>> 1 + (2 + 3) + 4 :: Expr
(1 + 2 + 3 + 4)

>>> 1 * (2 * 3) * 4 :: Expr
(1 * 2 * 3 * 4)

>>> 1 + (2 - 3) + 4 :: Expr
((1 + 2 + 4) - 3)

>>> 1 * (2 / 3) * 4 :: Expr
((1 * 2 * 4) / 3)

-}

module Puzzle.BadTwentyFour where

import Data.IntMultiSet (IntMultiSet)
import Data.Map (Map)
import Data.Semigroup (Semigroup (..), Endo (..))
import Data.Sequence (Seq, (|>), (<|))
import Text.Show (Show (..), showParen, showString)

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMultiSet as S
import qualified Data.Ratio as Ratio
import qualified Data.Sequence as Seq

type S = IntMultiSet

data Expr =
  Add (Seq Expr) |
  Mul (Seq Expr) |
  Sub Expr Expr |
  Div Expr Expr |
  Const Int

instance Num Expr
  where

    Add xs + Add ys = Add (xs <> ys)
    Sub w x + Sub y z = Sub (w + y) (x + z)
    Add xs + y = Add (xs |> y)
    y + Add xs = Add (y <| xs)
    Sub x y + z = Sub (x + z) y
    z + Sub x y = Sub (z + x) y
    x + y = Add $ Seq.fromList [x, y]

    Sub x y - Add zs = Sub x (y + Add zs)
    Add xs - Sub y z = Sub (Add xs + z) y
    Sub w x - Sub y z = Sub (w + z) (x + y)
    x - y = Sub x y

    Mul xs * Mul ys = Mul (xs <> ys)
    Div w x * Div y z = Div (w * y) (x * z)
    Mul xs * y = Mul (xs |> y)
    y * Mul xs = Mul (y <| xs)
    Div x y * z = Div (x * z) y
    z * Div x y = Div (z * x) y
    x * y = Mul $ Seq.fromList [x, y]

    fromInteger = Const . fromInteger

    signum = error "Expr does not support signum"
    abs    = error "Expr does not support abs"

instance Fractional Expr
  where

    Div x y / Mul zs = Div x (y * Mul zs)
    Mul xs / Div y z = Div (Mul xs * z) y
    Div w x / Div y z = Div (w * z) (x * y)
    x / y = Div x y

    fromRational x = Div (fromInteger $ Ratio.numerator x)
                         (fromInteger $ Ratio.denominator x)

instance Show Expr
  where
    showsPrec = exprShowsPrec

exprShowsPrec :: Int -> Expr -> ShowS
exprShowsPrec _ =
  \case
    Add xs -> exprShowsN '+' xs
    Mul xs -> exprShowsN '*' xs
    Sub x y -> exprShows2 '-' x y
    Div x y -> exprShows2 '/' x y
    Const x -> shows x

exprShowsN :: Char -> Seq Expr -> ShowS
exprShowsN op xs =
  showParen True .
  appEndo .
  Foldable.fold .
  List.intersperse (Endo $ showString [' ', op, ' ']) .
  fmap (Endo . shows) $
  Foldable.toList xs

exprShows2 :: Char -> Expr -> Expr -> ShowS
exprShows2 op x y =
  showParen True $
  shows x .
  showString [' ', op, ' '] .
  shows y

eval :: Expr -> Rational
eval =
  \case
    Add xs -> sum     $ fmap eval xs
    Mul xs -> product $ fmap eval xs
    Sub x y -> eval x - eval y
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
  , 2 * 2 * (2 + 2 + 2)
  , 3 * 3 * 3 - 3
  , 4 * 4 + 4 + 4
  , 5 * 5 - 5 / 5
  , 6 + 6 + 6 + 6
  , 7 + 7 + 7 + (7 + 7 + 7) / 7
  , 8 + 8 + 8
  , 9 + 9 + 9 - (9 + 9 + 9) / 9
  ]
