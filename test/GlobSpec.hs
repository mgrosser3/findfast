module GlobSpec (globSpec) where

import Control.Exception (IOException, throwIO, try)
import FindFast.Glob (match, pattern)
import System.Directory (doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Silently (capture)
import Test.Hspec

globSpec :: Spec
globSpec = do
  describe "test glob pattern matching" $ do
    --
    -- Simple Patterns
    -- Wildcards '*' and '?'
    --
    it "*.txt match test.txt" $ do
      let result = match (pattern "*.txt") "test.txt"
      result `shouldBe` True

    it "*test* match .txt" $ do
      let result = match (pattern "*test*") "test.txt"
      result `shouldBe` True

    it "* match README.md" $ do
      let result = match (pattern "*") "README.md"
      result `shouldBe` True

    it "*.txt doesn't match README.md" $ do
      let result = match (pattern "*.txt") "README.md"
      result `shouldBe` False

    it "*.txt doesn't match test.txt.backup" $ do
      let result = match (pattern "*.txt") "test.txt.backup"
      result `shouldBe` False

    it "test?.txt match test1.txt" $ do
      let result = match (pattern "test?.txt") "test1.txt"
      result `shouldBe` True

    it "test?.txt match testA.txt" $ do
      let result = match (pattern "test?.txt") "testA.txt"
      result `shouldBe` True

    it "test?.txt doesn't match test12.txt" $ do
      let result = match (pattern "test?.txt") "test12.txt"
      result `shouldBe` False

    it "test?.txt doesn't match test.txt" $ do
      let result = match (pattern "test?.txt") "test.txt"
      result `shouldBe` False

    it "?.txt match a.txt" $ do
      let result = match (pattern "?.txt") "a.txt"
      result `shouldBe` True

    it "?.txt doesn't match ab.txt" $ do
      let result = match (pattern "?.txt") "ab.txt"
      result `shouldBe` False

    --
    -- Pattern with Character Ranges
    --
    --
    it "test[0-9].txt match test5.txt" $ do
      let result = match (pattern "test[0-9].txt") "test5.txt"
      result `shouldBe` True

    it "test[0-9].txt match test7.txt" $ do
      let result = match (pattern "test[0-9].txt") "test7.txt"
      result `shouldBe` True
