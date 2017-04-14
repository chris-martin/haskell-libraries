{- |

Worksheet for other state(s) tax credit, full-year residents.

From IT 511, the top half of page 15.

This doesn't output the entire worksheet because I have not bothered to codify
the entire tax table, but it takes care of most of the dull arithmetic and
produces the values for close-to-last few lines, which you can then complete
manually after looking up your tax amount in the instruction book.

Example usage:

> result Input
>   { other_states_agi = 612430
>   , georgia_agi = 602410
>   , georgia_deductions = 2300
>   , georgia_form_500_line_14c = 2700
>   , tax_on_other_state_returns = 63826
>   }

-}

{-# LANGUAGE RecordWildCards #-}

module IRS.Year_2016.Georgia.Form_500.Worksheet_For_Other_States_Tax_Credit.Full_Year_Residents
  where

import Data.Ratio ((%))
import Prelude (Int, Num(..), Show, round)

data Input = Input

    { other_states_agi :: Int
    -- ^ Other state(s) adjusted gross income

    , georgia_agi :: Int
    -- ^ Georgia adjusted gross icome (Line 10, Form 500)

    , georgia_deductions :: Int
    -- ^ Georgia standard or itemized deductions

    , georgia_form_500_line_14c :: Int
    -- ^ Georgia personal exemption and credit for dependents from Form 500,
    -- Line 14c

    , tax_on_other_state_returns :: Int
    -- ^ Tax shown on return(s) filed with other state(s). The amount entered
    -- must be reduced by the credits that have been allowed by the other
    -- states.

    } deriving Show

data Output = Output

    { output_line_8 :: Int
    -- ^ Income for computation of credit

    , output_line_10 :: Int
    -- ^ Tax shown on return(s) filed with other state(s)

    } deriving Show

result :: Input -> Output
result Input{..} =
  let

    line_1 = other_states_agi

    line_2 = georgia_agi

    line_3 = line_1 % line_2

    line_4 = georgia_deductions

    line_5 = georgia_form_500_line_14c

    line_6 = line_4 + line_5

    line_7 = round ((line_6 % 1) * line_3)

    line_8 = line_1 - line_7

    line_10 = tax_on_other_state_returns

  in
    Output
        { output_line_8 = line_8
        , output_line_10 = line_10
        }
