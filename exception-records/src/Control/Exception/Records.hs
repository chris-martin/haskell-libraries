{- |

This package is a thin layer over the /safe-exceptions/
("Control.Exception.Safe") utilities for handling exceptions, using record
syntax to provide an alternative way of calling these functions that you may
find more readable.

Example:

> import Control.Exception.Records
>
> -- TODO: put a @Bracket@ example here.

At its core is the 'Run' class:

> class Run a m r where
>     run :: r -> m a

Types with @Run@ instances are categorized into three groups based on
what they do with synchronous exceptions:

* An exception-/swallowing/ runnable
  ("Control.Exception.Records.SwallowException")
  does not throw an exception that comes from its @action@; it has a @catch@
  phase which produces a value of type @m a@, allowing 'run' to "recover" from
  an exception in the @action@ and still produce a value.
* An exception-/propagating/ runnable
  ("Control.Exception.Records.PropagateException")
  does throw any exception that its @action@ throws, but first it catches the
  exception and does some @cleanup@ work before re-throwing it. Many of these
  also include a preliminary @setup@ phase which produces some resource that
  the @cleanup@ must release; this pattern is called "bracket".
* An exception-/reifying/ runnable
  ("Control.Exception.Records.ReifyException")
  can catch an exception from its @action@ and produce it an an output value.
  The result type of its 'run' method is @m ('Either' ex a)@, where @ex@ is the
  type of exception that was caught. This pattern is called "try".

Name conventions for type variables:

* The type of an @action@ is @m a@.
* @ex@ is an exception.
* @res@ is a resource produced by the @setup@ phase of an exception-propagating
  runnable and cleaned up by its @cleanup@ phase.
* @b@ is a throwaway value.

-}

module Control.Exception.Records
    (
    -- * Classes

      Run      (..)
    , RunDeep  (..)
    , RunAsync (..)

    -- * Exception-swallowing ("catch")

    , Catch     (..)
    , CatchIO   (..)
    , CatchAny  (..)
    , CatchJust (..)
    , Catches   (..)
    , Handler   (..)

    -- * Exception-propagating ("bracket", "finally")

    , OnException     (..)
    , Bracket         (..)
    , Bracket_        (..)
    , Finally         (..)
    , WithException   (..)
    , BracketOnError  (..)
    , BracketOnError_ (..)

    -- * Exception-reifying ("try")

    , Try     (..)
    , TryIO   (..)
    , TryAny  (..)
    , TryJust (..)
    , Either  (..)

    ) where

import Control.Exception.Records.Class
import Control.Exception.Records.PropagateException
import Control.Exception.Records.ReifyException
import Control.Exception.Records.SwallowException
