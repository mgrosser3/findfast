module ProcessPathSpec (processPathSpec) where

import Control.Exception (IOException, throwIO, try)
import FindFast.ProcessPath (processPath)
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
