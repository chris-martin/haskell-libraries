{- |

Provides 'AtomicModify', a typeclass for mutable references that have an atomic
modify operations. This generalizes atomic modify operations in 'IO' and 'STM'
contexts for 'IORef', 'MVar', 'TVar', and 'TMVar'.

* 'IORef' and 'MVar' can be modified in 'IO'.
* 'TVar' and 'TMVar' can be modified in 'IO' or 'STM'.

-}

{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module Control.Concurrent.AtomicModify
  ( AtomicModify (..)
  , atomicModifyStrict_
  , atomicModifyLazy_
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Concurrent.STM (STM, TMVar, TVar, atomically, putTMVar, readTVar,
                               takeTMVar, writeTVar)
import Control.Monad ((>>=))
import Data.Function (($), (&))
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef, atomicModifyIORef')
import Prelude (IO, pure, ($!))


--------------------------------------------------------------------------------
--  Class
--------------------------------------------------------------------------------

{- |

A typeclass for mutable references that have an atomic modify operation.

Type variables:

* @ref@ - The reference (e.g. 'IORef', 'TVar', 'MVar', 'TMVar')
* @m@ - The monad in which the modification takes place (e.g. 'IO', 'STM')

As the name "atomic" implies, these functions are useful for using mutable
references in a safe way to prevent race conditions in a multithreaded
program.

-}
class AtomicModify ref m where

  {- |

  Atomically modify the contents of a @ref@ (type @a@) and produce a value (type
  @b@). This is strict; it forces the value stored in the @ref@ as well as the
  value returned.

  -}
  atomicModifyStrict :: ref a -> (a -> (a, b)) -> m b

  {- |

  Atomically modify the contents of a @ref@ (type @a@) and produce a value (type
  @b@). This is lazy, which means if the program calls 'atomicModifyLazy' many
  times, but seldomly uses the value, thunks will pile up in memory resulting in
  a space leak.

  -}
  atomicModifyLazy :: ref a -> (a -> (a, b)) -> m b


--------------------------------------------------------------------------------
--  Functions
--------------------------------------------------------------------------------

{- |

Atomically modify the contents of a @ref@. This is strict; it forces the value
stored in the @ref@ as well as the value returned.

-}
atomicModifyStrict_ :: AtomicModify v m => v a -> (a -> a) -> m ()
atomicModifyStrict_ ref f = atomicModifyStrict ref (\a -> (f a, ()))

{- |

Atomically modify the contents of a @ref@ (type @a@) and produce a value (type
@b@). This is lazy, which means if the program calls 'atomicModifyLazy_' many
times, but seldomly uses the value, thunks will pile up in memory resulting in a
space leak.

-}
atomicModifyLazy_ :: AtomicModify v m => v a -> (a -> a) -> m ()
atomicModifyLazy_ ref f = atomicModifyLazy ref (\a -> (f a, ()))


--------------------------------------------------------------------------------
--  Instances
--------------------------------------------------------------------------------

instance AtomicModify IORef IO
  where
    atomicModifyLazy   ref f = atomicModifyIORef  ref f
    atomicModifyStrict ref f = atomicModifyIORef' ref f

instance AtomicModify MVar IO
  where
    atomicModifyLazy   ref f = modifyMVar ref (\x -> pure $  f x)
    atomicModifyStrict ref f = modifyMVar ref (\x -> pure $! f x)

instance AtomicModify TVar STM
  where
    atomicModifyLazy ref f =
      readTVar ref >>= \a -> f a & \( a',  b) -> writeTVar ref a' $> b
    atomicModifyStrict ref f =
      readTVar ref >>= \a -> f a & \(!a', !b) -> writeTVar ref a' $> b

instance AtomicModify TMVar STM
  where
    atomicModifyLazy ref f =
      takeTMVar ref >>= \a -> f a & \( a',  b) -> putTMVar ref a' $> b
    atomicModifyStrict ref f =
      takeTMVar ref >>= \a -> f a & \(!a', !b) -> putTMVar ref a' $> b

instance AtomicModify TVar IO
  where
    atomicModifyLazy   ref f = atomically (atomicModifyLazy   ref f)
    atomicModifyStrict ref f = atomically (atomicModifyStrict ref f)

instance AtomicModify TMVar IO
  where
    atomicModifyLazy   ref f = atomically (atomicModifyLazy   ref f)
    atomicModifyStrict ref f = atomically (atomicModifyStrict ref f)
