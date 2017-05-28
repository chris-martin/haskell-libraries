{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Num.QQ
  ( num
  , int
  , nat
  ) where

import Control.Arrow ((>>>))
import Data.Bool ((||))
import Data.Char (isAlphaNum)
import Data.Eq ((==))
import Data.List (filter)
import Data.Function (const, ($), (.))
import Data.Semigroup
import Prelude (error, String, fromInteger, Num)
import Text.Read (Read, readsPrec)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

{- $setup

>>> import Prelude (Integer)

-}

{- |

>>> [num| 1,048,576 |] :: Integer
1048576

-}
num :: QuasiQuoter
num =
  qq "num" $
  filter (\c -> isAlphaNum c || c == '.' || c == '-') >>>
  (\s -> [| read "num" s :: (forall a. (Read a, Num a) => a) |])

{- |

>>> [int| 1,048,576 |] :: Integer
1048576

>>> [int| 1.048.576 |] :: Integer
1048576

>>> [int| 5e4 |] :: Integer
*** Exception: Invalid int
...

-}
int :: QuasiQuoter
int =
  qq "int" $
  filter (\c -> isAlphaNum c || c == '-') >>>
  (\s -> [| fromInteger (read "int" s) |])

{- |

>>> [nat| 123-45-6789 |] :: Integer
123456789

>>> [nat| 5,000 |] :: Integer
5000

>>> [nat| 5e4 |] :: Integer
*** Exception: Invalid nat
...

-}
nat :: QuasiQuoter
nat =
  qq "nat" $
  filter (\c -> isAlphaNum c) >>>
  (\s -> [| fromInteger (read "nat" s) |])

qq :: String -> (String -> ExpQ) -> QuasiQuoter
qq name exp =
  QuasiQuoter
    { quoteExp = exp
    , quotePat = const . error $ "No quotePat defined for " <> name
    , quoteType = const . error $ "No quoteType defined for " <> name
    , quoteDec = const . error $ "No quoteDec defined for " <> name
    }

read :: Read a => String -> String -> a
read name s =
  case readsPrec 0 s of
    [(x, "")] -> x
    _ -> error $ "Invalid " <> name
