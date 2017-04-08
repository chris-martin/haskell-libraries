{-# LANGUAGE FlexibleInstances, DuplicateRecordFields, MultiParamTypeClasses,
             RecordWildCards #-}

module Control.Exception.Records.PropagateException
    ( OnException     (..)
    , Bracket         (..)
    , Bracket_        (..)
    , Finally         (..)
    , WithException   (..)
    , BracketOnError  (..)
    , BracketOnError_ (..)
    ) where

import Control.Exception.Records.Class

import Control.Exception.Safe (MonadMask, Exception)

import qualified Control.Exception.Safe as E

--------------------------------------------------------------------------------

{- |
> data OnException b a m = OnException
>     { action  :: m a
>     , cleanup :: m b
>     }

Use with 'run'.
-}
data OnException b a m = OnException
    { action  :: m a
    , cleanup :: m b
    }

instance MonadMask m =>
    Run a m (OnException b a m)
  where
    run OnException{..} = E.onException action cleanup

--------------------------------------------------------------------------------

{- |
> data Bracket res b a m = Bracket
>     { setup   :: m res
>     , action  :: res -> m a
>     , cleanup :: res -> m b
>     }

Use with 'run'.
-}
data Bracket res b a m = Bracket
    { setup   :: m res
    , action  :: res -> m a
    , cleanup :: res -> m b
    }

instance MonadMask m =>
    Run a m (Bracket res b a m)
  where
    run Bracket{..} = E.bracket setup cleanup action

--------------------------------------------------------------------------------

{- |
> data Bracket_ res b a m = Bracket_
>     { setup   :: m res
>     , action  :: m a
>     , cleanup :: m b
>     }

Use with 'run'.
-}
data Bracket_ res b a m = Bracket_
    { setup   :: m res
    , action  :: m a
    , cleanup :: m b
    }

instance MonadMask m =>
    Run a m (Bracket_ res b a m)
  where
    run Bracket_{..} = E.bracket_ setup cleanup action

--------------------------------------------------------------------------------

{- |
> data Finally b a m = Finally
>     { action  :: m a
>     , cleanup :: m b
>     }

Use with 'run'.
-}
data Finally b a m = Finally
    { action  :: m a
    , cleanup :: m b
    }

instance MonadMask m =>
    Run a m (Finally b a m)
  where
    run Finally{..} = E.finally action cleanup

--------------------------------------------------------------------------------

{- |
> data WithException b ex a m = WithException
>     { action  :: m a
>     , cleanup :: ex -> m b
>     }

Use with 'run'.
-}
data WithException b ex a m = WithException
    { action  :: m a
    , cleanup :: ex -> m b
    }

instance (MonadMask m, Exception ex) =>
    Run a m (WithException b ex a m)
  where
    run WithException{..} = E.withException action cleanup

--------------------------------------------------------------------------------

{- |
> data BracketOnError res b a m = BracketOnError
>     { setup   :: m res
>     , action  :: res -> m a
>     , cleanup :: res -> m b
>     }

Use with 'run'.
-}
data BracketOnError res b a m = BracketOnError
    { setup   :: m res
    , action  :: res -> m a
    , cleanup :: res -> m b
    }

instance MonadMask m =>
    Run a m (BracketOnError res b a m)
  where
    run BracketOnError{..} = E.bracketOnError setup cleanup action

--------------------------------------------------------------------------------

{- |
> data BracketOnError_ res b a m = BracketOnError_
>     { setup   :: m res
>     , action  :: m a
>     , cleanup :: m b
>     }

Use with 'run'.
-}
data BracketOnError_ res b a m = BracketOnError_
    { setup   :: m res
    , action  :: m a
    , cleanup :: m b
    }

instance MonadMask m =>
    Run a m (BracketOnError_ b b a m)
  where
    run BracketOnError_{..} = E.bracketOnError_ setup cleanup action
