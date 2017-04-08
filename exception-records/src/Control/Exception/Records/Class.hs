{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Exception.Records.Class
    ( Run      (..)
    , RunDeep  (..)
    , RunAsync (..)
    ) where

class Run a m r where

    run :: r -> m a

class Run a m r => RunDeep a m r where

    -- | Same as 'run', but fully force evaluation of the result value to
    -- find all impure exceptions.
    runDeep :: r -> m a

class Run a m r => RunAsync a m r where

    -- | Same as 'run', but without async exception safety. This makes it
    -- possible to catch and recover from async exceptions. This generally
    -- should not be used, because a thread that receives an async exception
    -- is supposed to terminate promptly.
    runAsync :: r -> m a
