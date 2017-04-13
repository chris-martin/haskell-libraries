{-# LANGUAGE RecordWildCards #-}

{- |

This doesn't output the entire worksheet because I have not bothered to codify
the entire tax table, but it takes care of most of the dull arithmetic and
produces the values for close-to-last few lines, which you can then complete
manually after looking up your tax amount in the 1040 instruction book.

Example usage:

> result Input
>   { family_status = Single
>   , schedule_D = Just Schedule_D
>       { schedule_D_line_15 = Just (-2345)
>       , schedule_D_line_16 = Just (-2345)
>       }
>   , foreign_earned_income_worksheet = Nothing
>   , form_1040_line_43 = 218463
>   , form_1040_line_9b = 1247
>   , form_1040_line_13 = -2345
>   , form_4952_line_4g = Nothing
>   }

-}

module IRS.Year_2016.Federal.Form_1040.Qualified_Dividends_And_Capital_Gains_Worksheet
  where

import IRS.Prelude

data Family_Status =
      Single
    | Married_Filing_Separately
    | Married_Filing_Jointly  -- ^ Or qualifying widow(er)
    | Head_Of_Household
  deriving (Eq, Show)

data Schedule_D = Schedule_D
    { schedule_D_line_15 :: Maybe Integer
    , schedule_D_line_16 :: Maybe Integer
    } deriving Show

data Foreign_Earned_Income_Worksheet = Foreign_Earned_Income_Worksheet
    { foreign_earned_income_worksheet_line_3 :: Integer
    } deriving Show

data Input = Input

    { family_status :: Family_Status

    , schedule_D :: Maybe Schedule_D
    -- ^ Information from Schedule D, if you are filing it

    , foreign_earned_income_worksheet :: Maybe Foreign_Earned_Income_Worksheet
    -- ^ If you are filing form 2555 or 2555-EZ

    , form_1040_line_43 :: Integer
    -- ^ The amount from Form 1040, line 43

    , form_1040_line_9b :: Integer
    -- ^ The amount from Form 1040, line 9b

    , form_1040_line_13 :: Integer

    , form_4952_line_4g :: Maybe Integer

    } deriving Show

data Output = Output
    { output_line_7  :: Integer
    , output_line_20 :: Integer
    , output_line_23 :: Integer
    } deriving Show

result :: Input -> Output
result Input{..} =
  let

    line_1 = case foreign_earned_income_worksheet of
      Just w -> foreign_earned_income_worksheet_line_3 w
      Nothing -> form_1040_line_43

    line_2 = form_1040_line_9b

    line_3 = case schedule_D of
      Just Schedule_D{..} -> non_negative (min (or_zero schedule_D_line_15)
                                               (or_zero schedule_D_line_16))
      Nothing -> form_1040_line_13

    line_4 = line_2 + line_3

    line_5 = or_zero form_4952_line_4g

    line_6 = line_4 - line_5

    line_7 = line_1 - line_6

    line_8 = case family_status of
        Single                    -> 37650
        Married_Filing_Separately -> 37650
        Married_Filing_Jointly    -> 75300
        Head_Of_Household         -> 50400

    line_9 = min line_1 line_8

    line_10 = min line_7 line_9

    line_11 = line_9 - line_10

    line_12 = min line_1 line_6

    line_13 = line_11

    line_14 = line_12 - line_13

    line_15 = case family_status of
        Single                    -> 415050
        Married_Filing_Separately -> 233475
        Married_Filing_Jointly    -> 466950
        Head_Of_Household         -> 441000

    line_16 = min line_1 line_15

    line_17 = line_7 + line_11

    line_18 = non_negative (line_16 - line_17)

    line_19 = min line_14 line_18

    line_20 = round ((line_19 % 1) * (15 % 100))

    line_21 = line_11 + line_19

    line_22 = line_12 - line_21

    line_23 = round ((line_22 % 1) * (20 % 100))

  in
    Output
      { output_line_7 = line_7
      , output_line_20 = line_20
      , output_line_23 = line_23
      }
