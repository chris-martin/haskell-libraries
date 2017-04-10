{- |

I find it particularly embarrassing when details of a programming language leak
into a user interface, like when you see the word @undefined@ appear in a web
application. Statically typed languages are not entirely immune to this sort of
bug. In Haskell it's easy to make this mistake when you use 'show'.

For example, let's say we're showing an integer @i@:

> let i = 5 :: Integer
> displayInTheUI ("The number is " <> Text.pack (show i))

and we later refactor the code such that @i@ is now optional:

> let i = Just 5 :: Maybe Integer
> displayInTheUI ("The number is " <> Text.pack (show i))

We've forgotten to update what we show the user, but unfortunately it still
compiles, our user sees the text "Just 5", and their illusion that we can write
flawless code in Haskell is shattered.

Instead we should ensure we're showing them human text:

> let i = 5 :: Integer
> displayInTheUI ("The number is " <> humanText i)

and this does not compile:

> let i = Just 5 :: Maybe Integer
> displayInTheUI ("The number is " <> humanText i)

-}

module Data.Text.Human
    (
    -- * The @HumanText@ class
      HumanText (..)

    -- * Instances

    -- ** Numbers
    -- $numbers

    -- ** Either
    -- $either
    ) where

import Data.Either (Either (..))
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Text (Text, pack)
import Data.Void (Void, absurd)
import Numeric.Natural (Natural)
import Prelude (Integer, show)

-- | An entirely unprincipled class for things which can be converted to text
-- that might be suitable to show to a human user.
class HumanText a where

    -- | Convert a value to some text suitable for displaying to a person. The
    -- output should probably not include Haskell or JSON or any other nerd
    -- shit.
    humanText :: a -> Text


--------------------------------------------------------------------------------
--  Void
--------------------------------------------------------------------------------

instance HumanText Void where
    humanText = absurd


--------------------------------------------------------------------------------
--  Either
--------------------------------------------------------------------------------

{- $either

If @a@ and @b@ both have human text representations, then the human text for
@'Either' a b@ is simply @humanText a@ or @humanText b@. Note that the output
won't necessarily reflect whether the value was 'Left' or 'Right'.

-}

instance (HumanText a, HumanText b) => HumanText (Either a b) where
    humanText (Left  a) = humanText a
    humanText (Right b) = humanText b


--------------------------------------------------------------------------------
--  Numbers
--------------------------------------------------------------------------------

{- $numbers

The integer types have 'HumanText' instances. The floating-point numbers don't,
because 0.1 + 0.2 = 0.30000000000000004 isn't human-friendly.

-}

instance HumanText Int where
    humanText i = pack (show i)

instance HumanText Int8 where
    humanText i = pack (show i)

instance HumanText Int16 where
    humanText i = pack (show i)

instance HumanText Int32 where
    humanText i = pack (show i)

instance HumanText Int64 where
    humanText i = pack (show i)

instance HumanText Integer where
    humanText i = pack (show i)

instance HumanText Natural where
    humanText i = pack (show i)
