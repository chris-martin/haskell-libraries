{-# LANGUAGE ScopedTypeVariables #-}

{- |

Hedgehog generators for types defined in the /loc/ package.

-}
module Test.Loc.Hedgehog.Gen
  (
  -- * Line
    line, line', defMaxLine

  -- * Column
  , column, column', defMaxColumn

  -- * Loc
  , loc, loc'

  -- * Span
  , span, span'

  -- * Area
  , area, area'

  -- * Generator bounds
  , Bounds, boundsSize

  ) where

import Data.Loc (ToNat (..))
import Data.Loc.Internal.Prelude
import Data.Loc.Types

import qualified Data.Loc as Loc

import Hedgehog (Gen)
import Prelude (Num (..))

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


--------------------------------------------------------------------------------
--  Parameter defaults
--------------------------------------------------------------------------------

-- | The default maximum line: 99.
defMaxLine :: Line
defMaxLine = 99

-- | The default maximum column number: 99.
defMaxColumn :: Column
defMaxColumn = 99


--------------------------------------------------------------------------------
--  Bounds
--------------------------------------------------------------------------------

-- | Inclusive lower and upper bounds on a range.
type Bounds a = (a, a)

{- |

The size of a range specified by 'Bounds'.

Assumes the upper bound is at least the lower bound.

-}
boundsSize :: Num n => (n, n) -> n
boundsSize (a, b) =
  1 + b - a


--------------------------------------------------------------------------------
--  Pos
--------------------------------------------------------------------------------

{- |

@'pos' a b@ generates a number on the linear range /a/ to /b/.

-}
pos :: (Monad m, ToNat n, Num n)
  => Bounds n -- ^ Minimum and maximum value to generate
  -> Gen m n
pos (a, b) =
  let
    range = Range.linear (toNat a) (toNat b)
  in
    fromInteger . toInteger <$> Gen.integral range

{- |

@'line' a b@ generates a line number on the linear range /a/ to /b/.

-}
line :: Monad m
  => Bounds Line -- ^ Minimum and maximum line number
  -> Gen m Line
line = pos

{- |

Generates a line number within the default bounds @(1, 'defMaxLine')@.

-}
line' :: Monad m => Gen m Line
line' =
  line (1, defMaxLine)

{- |

@'column' a b@ generates a column number on the linear range /a/ to /b/.

-}
column :: Monad m
  => Bounds Column -- ^ Minimum and maximum column number
  -> Gen m Column
column = pos

{- |

Generates a column number within the default bounds @(1, 'defMaxColumn')@.

-}
column' :: Monad m => Gen m Column
column' =
  column (1, defMaxColumn)


--------------------------------------------------------------------------------
--  Loc
--------------------------------------------------------------------------------

{- |

@'loc' lineBounds columnBounds@ generates a 'Loc' with the line number
bounded by @lineBounds@ and column number bounded by @columnBounds@.

-}
loc :: Monad m
  => Bounds Line   -- ^ Minimum and maximum line number
  -> Bounds Column -- ^ Minimum and maximum column number
  -> Gen m Loc
loc lineBounds columnBounds =
  Loc.loc <$> line   lineBounds
          <*> column columnBounds

{- |

Generates a 'Loc' within the default line and column bounds.

-}
loc' :: Monad m => Gen m Loc
loc' =
  loc (1, defMaxLine) (1, defMaxColumn)


--------------------------------------------------------------------------------
--  Span
--------------------------------------------------------------------------------

{- |

@'span' lineBounds columnBounds@ generates a 'Span' with start and end
positions whose line numbers are bounded by @lineBounds@ and whose column
numbers are bounded by @columnBounds@.

-}
span :: forall m. Monad m
  => Bounds Line   -- ^ Minimum and maximum line number
  -> Bounds Column -- ^ Minimum and maximum column number
  -> Gen m Span
span lineBounds columnBounds@(minColumn, maxColumn) =
  let
    lines :: Gen m (Line, Line)
    lines =
      line lineBounds >>= \a ->
      line lineBounds <&> \b ->
      (min a b, max a b)

    columnsDifferentLine :: Gen m (Column, Column)
    columnsDifferentLine =
      column columnBounds >>= \a ->
      column columnBounds <&> \b ->
      (a, b)

    columnsSameLine :: Gen m (Column, Column)
    columnsSameLine =
      column (minColumn + 1, maxColumn) >>= \a ->
      column columnBounds <&> \b ->
      case compare a b of
        EQ -> (a - 1, b)
        LT -> (a, b)
        GT -> (b, a)

  in
    lines >>= \(startLine, endLine) ->
    (if startLine /= endLine
        then columnsDifferentLine
        else columnsSameLine
    ) <&> \(startColumn, endColumn) ->

    let
      start = Loc.loc startLine startColumn
      end   = Loc.loc endLine   endColumn

    in
      Loc.spanFromTo start end

{- |

Generates a 'Span' with start and end positions within the default line and
column bounds.

-}
span' :: Monad m => Gen m Span
span' =
  span (1, defMaxLine) (1, defMaxColumn)


--------------------------------------------------------------------------------
--  Area
--------------------------------------------------------------------------------

{- |

@'area' lineBounds columnBounds@ generates an 'Area' consisting of 'Span's
with start and end positions whose line numbers are bounded by @lineBounds@
and whose column numbers are bounded by @columnBounds@.

-}
area :: forall m. Monad m
  => Bounds Line   -- ^ Minimum and maximum line number
  -> Bounds Column -- ^ Minimum and maximum column number
  -> Gen m Area
area lineBounds columnBounds =
    fold . snd . mapAccumL f Nothing . Set.toAscList . Set.fromList <$> locs

  where
    gridSize :: Int = fromIntegral $ toNat (boundsSize lineBounds)
                               `max` toNat (boundsSize columnBounds)

    locs :: Gen m [Loc]
      = loc lineBounds columnBounds
      & List.repeat
      & List.take (gridSize `div` 5)
      & sequenceA

    f :: Maybe Loc -> Loc -> (Maybe Loc, Area)
    f prevLocMay newLoc =
      case prevLocMay of
        Just prevLoc -> (Nothing, Loc.areaFromTo prevLoc newLoc)
        Nothing -> (Just newLoc, mempty)

{- |

Generates an 'Area' consisting of 'Span's with start and end positions within
the default line and column bounds.

-}
area' :: Monad m => Gen m Area
area' =
  area (1, defMaxLine) (1, defMaxColumn)
