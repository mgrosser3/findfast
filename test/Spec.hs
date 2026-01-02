module Main (main) where

import ProcessPathSpec (matchSpec, processPathSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  processPathSpec
  matchSpec
