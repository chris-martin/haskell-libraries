{- |

Recommended import:

> import Data.Loc.Types
> import qualified Data.Loc as Loc

This module contains most of the interesting aspects of the library, but you
may also want to look at "Data.Loc.Loc", "Data.Loc.Span", and "Data.Loc.Area".

-}

module Data.Loc
    (
    -- * Core types
      Line, Column, Loc, Span, Area

    -- * Constructing
    , loc, origin, spanFromTo, spanFromToMay, areaFromTo, spanArea

    -- * Deconstructing
    , areaSpansAsc
    , spanStart, spanEnd

    -- * Combining
    , areaUnion, areaDifference
    , spanUnion, spanDifference

    -- * Miscellaneous
    , Pos, OneToTwo, ZeroToTwo, ToNat (..), LocException (..)

    ) where

import Data.Loc.Internal.Prelude

import Data.Loc.Area (Area, spanArea)
import Data.Loc.Exception (LocException (..))
import Data.Loc.List.OneToTwo (OneToTwo)
import Data.Loc.List.ZeroToTwo (ZeroToTwo)
import Data.Loc.Loc (Loc, loc, origin)
import Data.Loc.Pos (Column, Line, Pos, ToNat (..))
import Data.Loc.Span (Span)

import qualified Data.Loc.Area as Area
import qualified Data.Loc.Span as Span

-- | See 'Span.fromTo'.
spanFromTo :: Loc -> Loc -> Span
spanFromTo = Span.fromTo

-- | See 'Span.fromToMay'.
spanFromToMay :: Loc -> Loc -> Maybe Span
spanFromToMay = Span.fromToMay

-- | See 'Area.fromTo'.
areaFromTo :: Loc -> Loc -> Area
areaFromTo = Area.fromTo

-- | See 'Area.+'.
areaUnion :: Area -> Area -> Area
areaUnion = (Area.+)

-- | See 'Area.-'.
areaDifference :: Area -> Area -> Area
areaDifference = (Area.-)

-- | See 'Area.spansAsc'.
areaSpansAsc :: Area -> [Span]
areaSpansAsc = Area.spansAsc

-- | See 'Span.+'.
spanUnion :: Span -> Span -> OneToTwo Span
spanUnion = (Span.+)

-- | See 'Span.-'.
spanDifference :: Span -> Span -> ZeroToTwo Span
spanDifference = (Span.-)

-- | See 'Span.start'.
spanStart :: Span -> Loc
spanStart = Span.start

-- | See 'Span.end'.
spanEnd :: Span -> Loc
spanEnd = Span.end
