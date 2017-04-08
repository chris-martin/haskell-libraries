{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, MultiParamTypeClasses,
             RecordWildCards #-}

module Control.Exception.Records.ReifyException
    ( Try     (..)
    , TryIO   (..)
    , TryAny  (..)
    , TryJust (..)
    , Either  (..)
    ) where

import Control.Exception.Records.Class

import Control.DeepSeq (NFData)
import Control.Exception.Safe (MonadCatch, Exception, IOException,
                               SomeException)
import Control.Monad.IO.Class (MonadIO)
import Data.Either (Either (..))
import Data.Maybe (Maybe)

import qualified Control.Exception.Safe as E

--------------------------------------------------------------------------------

{- |
> data Try ex a m = Try
>     { action :: m a
>     }

Use with 'run', 'runDeep', or 'runAsync'.
-}
data Try ex a m = Try
    { action :: m a
    }

instance (MonadCatch m, Exception ex) =>
    Run (Either ex a) m (Try ex a m)
  where
    run Try{..} = E.try action

instance (MonadCatch m, MonadIO m, Exception ex, NFData a) =>
    RunDeep (Either ex a) m (Try ex a m)
  where
    runDeep Try{..} = E.tryDeep action

instance (MonadCatch m, Exception ex) =>
    RunAsync (Either ex a) m (Try ex a m)
  where
    runAsync Try{..} = E.tryAsync action

--------------------------------------------------------------------------------

{- |
> data TryIO a m = TryIO
>     { action :: m a
>     }

Use with 'run'.
-}
data TryIO a m = TryIO
    { action :: m a
    }

instance MonadCatch m =>
    Run (Either IOException a) m (TryIO a m)
  where
    run TryIO{..} = E.tryIO action

--------------------------------------------------------------------------------

{- |
> data TryAny a m = TryAny
>     { action :: m a
>     }

Use with 'run' or 'runDeep'.
-}
data TryAny a m = TryAny
    { action :: m a
    }

instance MonadCatch m =>
    Run (Either SomeException a) m (TryAny a m)
  where
    run TryAny{..} = E.tryAny action

instance (MonadCatch m, MonadIO m, NFData a) =>
    RunDeep (Either SomeException a) m (TryAny a m)
  where
    runDeep TryAny{..} = E.tryAnyDeep action

--------------------------------------------------------------------------------

{- |
> data TryJust b ex a m = TryJust
>     { action    :: m a
>     , predicate :: ex -> Maybe b
>     }

Use with 'run'.
-}
data TryJust b ex a m = TryJust
    { action    :: m a
    , predicate :: ex -> Maybe b
    }

instance (MonadCatch m, Exception ex) =>
    Run (Either b a) m (TryJust b ex a m)
  where
    run TryJust{..} = E.tryJust predicate action
