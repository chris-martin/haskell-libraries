module Data.Loc.Loc
  ( Loc

  -- * Constructing
  , loc
  , origin

  -- * Querying
  , line
  , column

  ) where

import Data.Loc.Pos (Column, Line)

import Data.Loc.Internal.Prelude

import qualified Text.ParserCombinators.ReadP as ReadP

-- | Stands for /location/. Consists of a 'Line' and a 'Column'. You can think
-- of a 'Loc' like a caret position in a text editor. Following the normal
-- convention for text editors and such, line and column numbers start with 1.
data Loc = Loc
  { line   :: Line
  , column :: Column
  }
  deriving (Eq, Ord)

-- |
-- >>> show (loc 3 14)
-- "3:14"
instance Show Loc
  where

    showsPrec _ (Loc l c) =
      shows l .
      (showString ":") .
      shows c

-- |
-- >>> read "3:14" :: Loc
-- 3:14
instance Read Loc
  where

    readPrec =
      Loc <$>
      readPrec <*
      (readP_to_Prec $ \_prec -> ReadP.char ':') <*>
      readPrec

-- | Create a 'Loc' from a line number and column number.
loc :: Line   -- ^ Line number (>= 1)
  -> Column -- ^ Column number (>= 1)
  -> Loc
loc = Loc

-- | The smallest location: @'loc' 1 1@.
--
-- >>> origin
-- 1:1
origin :: Loc
origin = loc 1 1
