{- |

https://stackoverflow.com/questions/45203121

>>> solve ""
0

>>> solve "a"
1

>>> solve "ab"
2

>>> solve "abc"
2

>>> solve "aba"
3

>>> solve "abca"
3

>>> solve "beabeefeab"
5

>>> solve "cabacbxabac"
7

>>> solve "abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"
4

-}

module AlternatingCharacters
  ( solve
  ) where

import Data.Eq (Eq (..))
import Numeric.Natural (Natural)
import Prelude (max, filter, succ)

{- |

@solve xs@ gives the solution for string @xs@.

-}
solve :: Eq a => [a] -> Natural
solve [] = 0
solve (x:xs) =
  max
    (solve (filter (/= x) xs))
    (solve' x xs)

{- |

@solve' a xs@ gives the solution for string @a:xs@, assuming that the maximum
alternation starts with @a@.

-}
solve' :: Eq a => a -> [a] -> Natural
solve' _ [] = 1
solve' a (x:xs) =
  max
    (solve' a (filter (/= x) xs))
    (solve'' 2 a x xs)

{- |

@solve'' n a b xs@ gives @n@ plus the solution for string @zs ++ xs@, where @zs@
is an alternation of length @n@ ending in @[a, b]@, assuming that the maximum
alternation contains the letters @a@ and @b@.

-}
solve'' :: Eq a => Natural -> a -> a -> [a] -> Natural
solve'' n _ _ [] = n
solve'' n a b (x:xs) =
  if x == a
    then solve'' (succ 1) b a xs
    else solve'' n a b xs
