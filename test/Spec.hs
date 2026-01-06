module Main (main) where

import GlobSpec (globSpec)
import ProcessPathSpec (processPathSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  processPathSpec
  globSpec
