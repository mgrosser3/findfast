module GlobSpec (globSpec) where

import Control.Exception (IOException, throwIO, try)
import FindFast.Glob (matches)
import System.Directory (doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Silently (capture)
import Test.Hspec

globSpec :: Spec
globSpec = do
  describe "test glob pattern matching" $ do
    -- Simple Patterns
    it "*.txt matches test.txt" $ do
      let result = "*.txt" `matches` "test.txt"
      result `shouldBe` True

    it "*test* matches .txt" $ do
      let result = "*test*" `matches` "test.txt"
      result `shouldBe` True

    it "* matches README.md" $ do
      let result = "*" `matches` "README.md"
      result `shouldBe` True

    it "*.txt doesn't match README.md" $ do
      let result = "*.txt" `matches` "README.md"
      result `shouldBe` False

    it "*.txt doesn't match test.txt.backup" $ do
      let result = "*.txt" `matches` "test.txt.backup"
      result `shouldBe` False

    it "test?.txt matches test1.txt" $ do
      let result = "test?.txt" `matches` "test1.txt"
      result `shouldBe` True

    it "test?.txt matches testA.txt" $ do
      let result = "test?.txt" `matches` "testA.txt"
      result `shouldBe` True

    it "test?.txt doesn't match test12.txt" $ do
      let result = "test?.txt" `matches` "test12.txt"
      result `shouldBe` False

    it "test?.txt doesn't match test.txt" $ do
      let result = "test?.txt" `matches` "test.txt"
      result `shouldBe` False

    it "?.txt matches a.txt" $ do
      let result = "?.txt" `matches` "a.txt"
      result `shouldBe` True

    it "?.txt doesn't match ab.txt" $ do
      let result = "?.txt" `matches` "ab.txt"
      result `shouldBe` False

    -- Pattern with Character Ranges
    it "test[0-9].txt matches test5.txt" $ do
      let result = "test[0-9].txt" `matches` "test5.txt"
      result `shouldBe` True

    it "test[0-9].txt matches test7.txt" $ do
      let result = "test[0-9].txt" `matches` "test7.txt"
      result `shouldBe` True

-- Combined Patterns
