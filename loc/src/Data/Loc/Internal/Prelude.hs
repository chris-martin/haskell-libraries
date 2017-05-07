module Data.Loc.Internal.Prelude
  ( module X
  , (<&>)
  ) where

import Control.Applicative as X (empty, pure, (*>), (<*), (<*>))
import Control.Arrow as X ((<<<), (>>>))
import Control.Exception as X (ArithException (..), Exception, throw)
import Control.Monad as X (Monad (..), guard, mfilter, when)
import Data.Bifunctor as X (Bifunctor (..))
import Data.Bool as X (Bool (..), not, otherwise, (&&), (||))
import Data.Eq as X (Eq (..))
import Data.Foldable as X (Foldable (..), foldMap, traverse_)
import Data.Function as X (flip, id, on, ($), (&), (.))
import Data.Functor as X (Functor (..), ($>), (<$), (<$>))
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Map as X (Map)
import Data.Maybe as X (Maybe (..), catMaybes, maybe)
import Data.Monoid as X (Monoid (..))
import Data.Ord as X (Ord (..), Ordering (..), max, min)
import Data.Semigroup as X (Semigroup (..))
import Data.Set as X (Set)
import Data.Traversable as X (mapAccumL, sequenceA, traverse)
import Data.Tuple as X (fst, snd)
import Numeric.Natural as X (Natural)
import Prelude as X (Double, Enum (..), Int, Integral, Real (..), div,
                     fromIntegral, print, quotRem, round, sqrt, toInteger,
                     undefined, (/))
import System.Exit as X (exitFailure)
import System.IO as X (IO)
import Text.ParserCombinators.ReadPrec as X (readP_to_Prec)
import Text.Read as X (Read (..), read)
import Text.Show as X (Show (..), showString, shows)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
