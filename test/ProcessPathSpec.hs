module ProcessPathSpec (processPathSpec, matchSpec) where

import Control.Exception (IOException, throwIO, try)
import FindFast.ProcessPath (match, processPath)
import System.Directory (doesFileExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO.Silently (capture)
import Test.Hspec

processPathSpec :: Spec
processPathSpec = do
  describe "test function processPath" $ do
    it "print file path" $ do
      path <- makeAbsolute "./test/data/test-folder-01/01_lorem_40000_words.txt"
      (output, _) <-
        capture $
          processPath printFilePath printDirectoryPath path
      output `shouldContain` "File: "
      output `shouldContain` "01_lorem_40000_words.txt"
    it "print pathes of files and directories" $ do
      path <- makeAbsolute "./test/data/test-folder-01"
      (output, _) <-
        capture $
          processPath printFilePath printDirectoryPath path
      output `shouldContain` "Directory: "
      output `shouldContain` "test-folder-01"
  where
    printFilePath :: String -> IO ()
    printFilePath path = putStrLn $ "File: " ++ path
    printDirectoryPath :: String -> IO ()
    printDirectoryPath path = putStrLn $ "Directory: " ++ path

matchSpec :: Spec
matchSpec = do
  describe "test glob pattern matching" $ do
    -- Simple Patterns
    it "*.txt matches test.txt" $ do
      let result = match "*.txt" "test.txt"
      result `shouldBe` True

    it "*test* matches .txt" $ do
      let result = match "*test*" "test.txt"
      result `shouldBe` True

    it "* matches README.md" $ do
      let result = match "*" "README.md"
      result `shouldBe` True

    it "*.txt doesn't match README.md" $ do
      let result = match "*.txt" "README.md"
      result `shouldBe` False

    it "*.txt doesn't match test.txt.backup" $ do
      let result = match "*.txt" "test.txt.backup"
      result `shouldBe` False

    it "test?.txt matches test1.txt" $ do
      let result = match "test?.txt" "test1.txt"
      result `shouldBe` True

    it "test?.txt matches testA.txt" $ do
      let result = match "test?.txt" "testA.txt"
      result `shouldBe` True

    it "test?.txt doesn't match test12.txt" $ do
      let result = match "test?.txt" "test12.txt"
      result `shouldBe` False

    it "test?.txt doesn't match test.txt" $ do
      let result = match "test?.txt" "test.txt"
      result `shouldBe` False

    it "?.txt matches a.txt" $ do
      let result = match "?.txt" "a.txt"
      result `shouldBe` True

    it "?.txt doesn't match ab.txt" $ do
      let result = match "?.txt" "ab.txt"
      result `shouldBe` False

    -- Pattern with Character Ranges
    it "test[0-9].txt matches test5.txt" $ do
      let result = match "test[0-9].txt" "test5.txt"
      result `shouldBe` True

    it "test[0-9].txt matches test7.txt" $ do
      let result = match "test[0-9].txt" "test7.txt"
      result `shouldBe` True

-- Combined Patterns
