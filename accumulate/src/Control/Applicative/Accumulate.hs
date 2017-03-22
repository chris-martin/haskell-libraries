{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, FlexibleContexts,
             FlexibleInstances, MultiParamTypeClasses, RankNTypes,
             ScopedTypeVariables, TypeApplications, TypeFamilies #-}

module Control.Applicative.Accumulate
    (
    -- * Acc
    Acc, accLiftA2, accTraverse, accSequence, accFold

    -- * Catch
    , accCatchAp, accCatchLiftA2, accCatchTraverse
    ) where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Data.Either
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Semigroup
import Data.Traversable


-- | A class for types which can be converted to/from a type that has an
-- @e@-accumulating 'Applicative'.
class Applicative (Acc' f) => Acc e f where
    type Acc' f :: * -> *
    acc :: f a -> Acc' f a
    unAcc :: Acc' f a -> f a

-- | See 'liftA2'.
accLiftA2 :: forall e f a b c. Acc e f
    => (a -> b -> c) -> f a -> f b -> f c
accLiftA2 f a b = unAcc @e $ liftA2 f (acc @e a) (acc @e b)

-- | See 'traverse'.
accTraverse :: forall e f a b t. (Acc e f, Traversable t)
    => (a -> f b) -> t a -> f (t b)
accTraverse f = unAcc @e . traverse (acc @e . f)

-- | See 'sequence'.
accSequence :: forall e f a t. (Acc e f, Traversable t)
    => t (f a) -> f (t a)
accSequence = unAcc @e . traverse (acc @e)

-- | See 'fold'.
accFold :: forall e f a t. (Acc e f, Foldable t, Monoid (Acc' f a))
    => t (f a) -> f a
accFold = unAcc @e . foldMap (acc @e)


--------------------------------------------------------------------------------
--  Maybe
--------------------------------------------------------------------------------

-- | 'Maybe' throws away all of its errors, so the accumulating behavior isn't
-- any different, so the accumulating version of 'Maybe' is itself.
instance Acc e Maybe where
    type Acc' Maybe = Maybe
    acc = id
    unAcc = id


--------------------------------------------------------------------------------
--  Either
--------------------------------------------------------------------------------

newtype AccEither e a = AccEither { unAccEither :: Either e a }
    deriving Functor

instance Semigroup e => Applicative (AccEither e) where
    pure = AccEither . pure
    AccEither f <*> AccEither a = AccEither (accEitherAp f a)

instance (Semigroup e, Monoid a) => Monoid (AccEither e a) where
    mempty = AccEither (Right mempty)
    AccEither x `mappend` AccEither y = AccEither (x `accEitherMappend` y)

instance Semigroup e => Acc e (Either e) where
    type Acc' (Either e) = AccEither e
    acc = AccEither
    unAcc = unAccEither

-- | Left-accumulating implementation of '(<*>)' for 'Either'.
accEitherAp :: forall e a b. Semigroup e
    => Either e (a -> b) -> Either e a -> Either e b
Left e1 `accEitherAp` Left e2 = Left (e1 <> e2)
Left e1 `accEitherAp` Right _ = Left e1
Right _ `accEitherAp` Left e2 = Left e2
Right f `accEitherAp` Right a = Right (f a)

-- | Left-accumulating implementation of 'mappend' for 'Either'.
accEitherMappend :: forall e a. (Semigroup e, Monoid a)
    => Either e a -> Either e a -> Either e a
Left e1 `accEitherMappend` Left e2 = Left (e1 <> e2)
Left e1 `accEitherMappend` Right _ = Left e1
Right _ `accEitherMappend` Left e2 = Left e2
Right a `accEitherMappend` Right b = Right (mappend a b)


--------------------------------------------------------------------------------
--  Catch
--------------------------------------------------------------------------------

newtype AccCatch e m a = AccCatch { unAccCatch :: m a } deriving Functor

instance (Semigroup e, Exception e, MonadCatch m)
        => Applicative (AccCatch e m) where
    pure = AccCatch . pure
    AccCatch f <*> AccCatch x = AccCatch (accCatchAp @e @m f x)

-- This stuff is doesn't quite unify with the 'Acc' class. It's unclear whether
-- it can.
{-
instance (Semigroup e, Exception e, MonadCatch m) => Acc e m where
    type Acc' m = AccCatch e m  -- Problem on this line
    acc = AccCatch @e @m
    unAcc = unAccCatch @e @m
-}

-- | @e@-accumulating implementation of '(<*>)' for a 'Monad' that can catch
-- exceptions.
accCatchAp :: forall e m a b.
    (Semigroup e, Exception e, MonadCatch m) =>
    m (a -> b) -> m a -> m b
accCatchAp mf ma = do
    fEither <- try @m @e mf
    aEither <- try @m @e ma
    either throwM pure (accEitherAp fEither aEither)

accCatchLiftA2 :: forall e m a b c.
    (Semigroup e, Exception e, MonadCatch m) =>
    (a -> b -> c) -> m a -> m b -> m c
accCatchLiftA2 f a b = accCatchAp @e (f <$> a) b

accCatchTraverse :: forall e m a b t.
    (Semigroup e, Exception e, MonadCatch m, Traversable t)
    => (a -> m b) -> t a -> m (t b)
accCatchTraverse f = unAccCatch @e . traverse (AccCatch @e . f)
