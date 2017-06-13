module Main (main) where

import Control.Concurrent
import Data.Functor
import Prelude

main :: IO ()
main = void (forkOS (putStrLn "test"))
