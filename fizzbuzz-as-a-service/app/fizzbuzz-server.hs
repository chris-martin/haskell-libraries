{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString)
import Data.Semigroup ((<>))
import Data.String (fromString)
import Numeric.Natural (Natural)

import qualified Network.Simple.TCP as Net

import qualified Options.Applicative as Opt

main :: IO ()
main =
  getArgs >>= \args ->
  Net.serve Net.HostAny (argPort args) accept

accept :: (Net.Socket, Net.SockAddr) -> IO ()
accept (s, _) =
    go 1
  where
    go i =
      Net.send s (fizzbuzz i) *>
      Net.send s "\n" *>
      threadDelay 500000 *>
      go (succ i)

fizzbuzz :: Natural -> ByteString
fizzbuzz i =
  case (i `mod` 3 == 0, i `mod` 5 == 0) of
    (True,  False) -> "Fizz"
    (False, True)  -> "Buzz"
    (True,  True)  -> "Fizz buzz"
    (False, False) -> fromString (show i)

data Args = Args
  { argPort :: String
  }

getArgs :: IO Args
getArgs =
  Opt.execParser $
  Opt.info (Opt.helper <*> argP) (Opt.header "FizzBizz as a service.")

argP :: Opt.Parser Args
argP = Args <$> portP

portP :: Opt.Parser Net.ServiceName
portP =
  Opt.strOption $
  Opt.long "port" <>
  Opt.short 'p' <>
  Opt.metavar "PORT" <>
  Opt.help "Service port to bind."
