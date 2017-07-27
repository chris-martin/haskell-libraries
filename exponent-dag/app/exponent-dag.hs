module Main (main) where

import Control.Applicative (pure)
import Data.Foldable (foldr)
import Data.Functor (void)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.GraphViz.Types.Monadic (digraph', (-->))
import Data.GraphViz.Commands (runGraphviz, GraphvizOutput (..))
import Prelude
import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= \args ->
  case args of
    [maxNodeStr, out] -> void $ runGraphviz (graph (read maxNodeStr)) Svg out
    _ -> error "bad args"

graph :: Integer -> DotGraph Integer
graph maxNode =
  digraph' $ foldr (*>) (pure ()) ((\(a, b) -> a --> b) <$> edges maxNode)

edges :: Integer -> [(Integer, Integer)]
edges maxNode =
  foldMap
    (\a ->
        (fmap $ \b -> (a, b))
      . takeWhile (<= maxNode)
      . fmap (\i -> a + 2 ^ (i :: Integer))
      $ [0..]
    )
    [0 .. maxNode - 1]
