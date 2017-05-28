import Prelude
import Test.DocTest

main :: IO ()
main =
  doctest ["-XNoImplicitPrelude", "-XQuasiQuotes", "src"]
