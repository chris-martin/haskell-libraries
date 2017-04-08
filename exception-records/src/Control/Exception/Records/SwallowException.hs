{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, MultiParamTypeClasses,
             RecordWildCards #-}

module Control.Exception.Records.SwallowException
    ( Catch     (..)
    , CatchIO   (..)
    , CatchAny  (..)
    , CatchJust (..)

    -- * Handling multiple types of exception
    , Catches (..)
    , Handler (..)

    ) where

import Control.Exception.Records.Class

import Control.DeepSeq (NFData)
import Control.Exception.Safe (Exception, Handler (..), IOException,
                               MonadCatch, MonadThrow, SomeException)
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (Maybe)

import qualified Control.Exception.Safe as E

--------------------------------------------------------------------------------

{- |
@catch@ is a handler for exceptions thrown during execution of @action@.
Note that the type of @ex@ constrains which exceptions are caught.

> data Catch ex a m = Catch
>     { action :: m a
>     , catch  :: ex -> m a
>     }

Use with 'run', 'runDeep', or 'runAsync'.

Example:

> run Catch
>     { action = readFile f
>     , catch  = \e -> do
>         let err = show (e :: IOException)
>         hPutStr stderr ("Warning: Couldn't open " <> f <> ": " <> err)
>         return ""
>     }
-}
data Catch ex a m = Catch
    { action ::       m a
    , catch  :: ex -> m a
    }

instance (MonadCatch m, Exception ex) =>
    Run a m (Catch ex a m)
  where
    run Catch{..} = E.catch action catch

instance (MonadCatch m, MonadIO m, Exception ex, NFData a) =>
    RunDeep a m (Catch ex a m)
  where
    runDeep Catch{..} = E.catchDeep action catch

instance (MonadCatch m, Exception ex) =>
    RunAsync a m (Catch ex a m)
  where
    runAsync Catch{..} = E.catchAsync action catch

--------------------------------------------------------------------------------

{- |
The same as 'Catch', but specialized to 'IOException's.

> data CatchIO a m = CatchIO
>     { action :: m a
>     , catch  :: IOException -> m a
>     }

Use with 'run'.

Example:

> run CatchIO
>     { action = readFile f
>     , catch  = \e -> do
>         let err = show e
>         hPutStr stderr ("Warning: Couldn't open " <> f <> ": " <> err)
>         return ""
>     }
-}
data CatchIO a m = CatchIO
    { action :: m a
    , catch  :: IOException -> m a
    }

instance MonadCatch m =>
    Run a m (CatchIO a m)
  where
    run CatchIO{..} = E.catchIO action catch

--------------------------------------------------------------------------------

{- |
The same as 'Catch', but specialized to 'SomeException' (which is the root of
the exception hierarchy, so this catches all exceptions).

> data CatchAny a m = CatchAny
>     { action :: m a
>     , catch  :: SomeException -> m a
>     }

Use with 'run' or 'runDeep'.
-}
data CatchAny a m = CatchAny
    { action :: m a
    , catch  :: SomeException -> m a
    }

instance MonadCatch m =>
    Run a m (CatchAny a m)
  where
    run CatchAny{..} = E.catchAny action catch

instance (MonadCatch m, MonadIO m, NFData a) =>
    RunDeep a m (CatchAny a m)
  where
    runDeep CatchAny{..} = E.catchAnyDeep action catch

--------------------------------------------------------------------------------

{- |
Like 'Catch', but with a @predicate@ that provides a more granular
specification of which exceptions to catch.

> data CatchJust b ex a m = CatchJust
>     { action    :: m a
>     , predicate :: ex -> Maybe b
>     , catch     :: b -> m a
>     }

Use with 'run'.
-}
data CatchJust b ex a m = CatchJust
    { action    :: m a
    , predicate :: ex -> Maybe b
    , catch     :: b -> m a
    }

instance (MonadCatch m, Exception ex) =>
    Run a m (CatchJust b ex a m)
  where
    run CatchJust{..} = E.catchJust predicate action catch

--------------------------------------------------------------------------------

{- |
> data Catches a m = Catches
>     { action ::          m a
>     , catch  :: [Handler m a]
>     }

Use with 'run', 'runDeep', or 'runAsync'.

Example:

> runDeep Catches
>     { action = (\s -> 500 `div` (read s :: Int)) <$> readFile "number.txt"
>     , catch =
>         [ Handler $ \(ex :: ArithException) -> do
>             hPutStr stderr ("Math error: " <> show ex)
>             pure 0
>         , Handler $ \(ex :: IOException) -> do
>             hPutStr stderr ("IO error: " <> show ex)
>             pure 0
>         ]
>     } :: IO Int
-}
data Catches a m = Catches
    { action :: m a
    , catch  :: [Handler m a]
    }

instance (MonadCatch m, MonadThrow m) =>
    Run a m (Catches a m)
  where
    run Catches{..} = E.catches action catch

instance (MonadCatch m, MonadThrow m, MonadIO m, NFData a) =>
    RunDeep a m (Catches a m)
  where
    runDeep Catches{..} = E.catchesDeep action catch

instance (MonadCatch m, MonadThrow m) =>
    RunAsync a m (Catches a m)
  where
    runAsync Catches{..} = E.catchesAsync action catch
