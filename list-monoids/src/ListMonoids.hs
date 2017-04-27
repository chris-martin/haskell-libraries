{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}

module ListMonoids
    (
    -- * Classes
      Semigroup (..)
    , Monoid (..)
    , Applicative (..)
    , Monad (..)
    , Foldable (..)
    , ListProduct (..)
    , ConsLike (..)

    -- * Types
    , List (..)
    , NonEmpty (..)
    , String (..)

    ) where

import Data.Char (Char)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Functor (Functor (..))

import qualified Prelude as P
import qualified Data.String as P (IsString (..))


--------------------------------------------------------------------------------
--  Classes
--------------------------------------------------------------------------------

class Semigroup a
  where

    (<>) :: a -> a -> a

class Semigroup a => Monoid a
  where

    mempty :: a

class Functor f => Applicative f
  where

    pure :: a -> f a

    (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f
  where

    join :: f (f a) -> f a

class Foldable f
  where

    toList :: f a -> List a

class Foldable f => ListProduct f
  where

    listProduct :: (a -> b -> c) -> f a -> f b -> f c
    listProduct f a b = listProduct' f a b b

    listProduct' :: (a -> b -> c)
                 -> f a
                 -> f b  -- ^ The total @b@ list
                 -> f b  -- ^ What's left of the "current" @b@ list
                 -> f c

class ConsLike f
  where

    (/) :: a -> List a -> f a

    infixr 5 /


--------------------------------------------------------------------------------
--  List
--------------------------------------------------------------------------------

data List a = Cons a (List a) | Nil
  deriving (Eq, Functor)

{- |

>>> Nil :: List String
List ()

>>> "1" / "2" / "3" / Nil :: List String
List ("1" / "2" / "3")

-}
instance P.Show a => P.Show (List a)
  where
    showsPrec = f
      where
        f _ Nil = ("List ()" P.++)
        f i (Cons x xs) = ("List (" P.++) . (P.showsPrec i x) . f' i xs

        f' i (Cons x xs) = (" / " P.++) . P.showsPrec i x . f' i xs
        f' _ Nil = (')' :)

{- |

>>> toList (Nil :: List String)
List ()

>>> toList ("1" / "2" / "3" / Nil :: List String)
List ("1" / "2" / "3")

-}
instance Foldable List
  where
    toList xs = xs

instance ConsLike List
  where
    (/) = Cons

instance ListProduct List
  where
    listProduct' _ Nil _ _ = Nil
    listProduct' _ _ Nil _ = Nil
    listProduct' f (Cons _ xs') ys Nil = listProduct' f xs' ys ys
    listProduct' f (Cons x xs) (Cons y ys) (Cons z zs) =
        toList (listProduct' f (NonEmpty x xs) (NonEmpty y ys) (NonEmpty z zs))

{- |

>>> Nil <*> Nil
List ()

>>> Nil <*> ("1" / "2" / "3" / Nil :: List String)
List ()

>>> (("a" <>) / ("b" <>) / ("c" <>) / Nil) <*> Nil :: List String
List ()

>>> (("a" <>) / ("b" <>) / ("c" <>) / Nil) <*> ("1" / "2" / Nil) :: List String
List ("a1" / "a2" / "b1" / "b2" / "c1" / "c2")

-}
instance Applicative List where
    pure x = Cons x Nil
    (<*>) = listProduct ($)

{- |

>>> Nil <> Nil :: List String
List ()

>>> Nil <> ("1" / "2" / "3" / Nil :: List String)
List ()

>>> (("a" <>) / ("b" <>) / ("c" <>) / Nil) <*> Nil :: List String
List ()

>>> ("a" / "b" / "c" / Nil) <> ("1" / "2" / Nil) :: List String
List ("a1" / "a2" / "b1" / "b2" / "c1" / "c2")

-}
instance Semigroup a => Semigroup (List a)
  where
    (<>) = listProduct (<>)

{- |

>>> mempty :: List String
List ("")

-}
instance Monoid a => Monoid (List a)
  where
    mempty = pure mempty


--------------------------------------------------------------------------------
--  NonEmpty
--------------------------------------------------------------------------------

data NonEmpty a = NonEmpty a (List a)
  deriving (Eq, Functor)

{- |

>>> "1" / "2" / "3" / Nil :: NonEmpty String
NonEmpty ("1" / "2" / "3")

-}
instance P.Show a => P.Show (NonEmpty a)
  where
    showsPrec = f
      where
        f i (NonEmpty x xs) = ("NonEmpty (" P.++) . (P.showsPrec i x) . f' i xs

        f' i (Cons x xs) = (" / " P.++) . P.showsPrec i x . f' i xs
        f' _ Nil = (')' :)

instance ConsLike NonEmpty
  where
    (/) = NonEmpty

{- |

>>> toList ("1" / "2" / "3" / Nil :: NonEmpty String)
List ("1" / "2" / "3")

-}
instance Foldable NonEmpty
  where
    toList (NonEmpty x xs) = Cons x xs

instance ListProduct NonEmpty
  where
    listProduct' f xs@(NonEmpty x _) ys (NonEmpty z zs') =
        NonEmpty (x `f` z) (listProduct' f (toList xs) (toList ys) zs')

{- |

>>> (("a" <>) / ("b" <>) / ("c" <>) / Nil) <*> ("1" / "2" / Nil) :: NonEmpty String
NonEmpty ("a1" / "a2" / "b1" / "b2" / "c1" / "c2")

-}
instance Applicative NonEmpty
  where
    pure x = NonEmpty x Nil
    (<*>) = listProduct ($)

{- |

>>> ("a" / "b" / "c" / Nil) <> ("1" / "2" / Nil) :: NonEmpty String
NonEmpty ("a1" / "a2" / "b1" / "b2" / "c1" / "c2")

-}
instance Semigroup a => Semigroup (NonEmpty a)
  where
    (<>) = listProduct (<>)

{- |

>>> mempty :: NonEmpty String
NonEmpty ("")

-}
instance Monoid a => Monoid (NonEmpty a)
  where
    mempty = pure mempty


--------------------------------------------------------------------------------
--  String
--------------------------------------------------------------------------------

newtype String = String [Char]
  deriving Eq

instance Semigroup String
  where
    String x <> String y = String (x P.++ y)

instance Monoid String
  where
    mempty = String ""

instance P.IsString String
  where
    fromString = String

instance P.Show String
  where
    show (String s) = P.show s
