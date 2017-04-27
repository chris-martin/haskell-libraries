import Prelude
import Test.DocTest

main :: IO ()
main = doctest ["src", "-XOverloadedStrings"]
