{- |

Using an @MVar ()@ as a lock is a common pattern. This module just wraps that up
into some functions with nice names that make the pattern explicit.

-}

module Control.Concurrent.MVarLock
    ( Lock, newLock, acquireLock, releaseLock, withLock
    ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception.Safe (finally)
import Prelude ((<$>), (*>), IO)

-- | A lock that can be exclusively acquired with 'acquireLock'.
newtype Lock = Lock (MVar ())

-- | Create a new lock.
newLock :: IO Lock
newLock = Lock <$> newMVar ()

-- | Block until the lock is available, then grab it. Something that acquires
-- the lock should at some point subsequently relinquish it with 'releaseLock'.
-- Consider using 'withLock' instead unless you need more fine-grained control.
acquireLock :: Lock -> IO ()
acquireLock (Lock v) = takeMVar v

-- | Release a lock that you have previously acquired with 'acquireLock'.
releaseLock :: Lock -> IO ()
releaseLock (Lock v) = putMVar v ()

-- | Acquire the lock, perform some action while the lock is held, then
-- release the lock. You can use this instead of manually calling 'acquireLock'
-- and 'releaseLock'.
withLock :: Lock -> IO a -> IO a
withLock lock action =
    (acquireLock lock *> action) `finally` (releaseLock lock)
