module ProcessPathSpec (processPathSpec) where

import FindFast.ProcessPath (processPath)
import Test.Hspec

processPathSpec :: Spec
processPathSpec = do
  describe "My first test" $ do
    it "calculate 1 + 1" $ do
      1 + 1 `shouldBe` 2
