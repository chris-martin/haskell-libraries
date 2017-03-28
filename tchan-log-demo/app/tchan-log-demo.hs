import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, forConcurrently_)
import Control.Exception (finally)
import Data.Char (toUpper)
import Prelude ((*), (*>), ($))
import System.IO (IO, hPutStr, stderr)

main :: IO ()
main = do
    a <- async $ forConcurrently_ ['a'..'z'] $ \c ->
        (print c *> delay 2) `finally` print (toUpper c)
    delay 1
    cancel a
  where
    delay sec = threadDelay (sec * 1000000)
    print c = hPutStr stderr [c]
