module Main (main) where

import ProcessPathSpec (processPathSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  processPathSpec
