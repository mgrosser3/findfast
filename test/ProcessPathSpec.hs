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
    it "print pathes of files and directories" $ do
      path <- makeAbsolute "./test/data/test-folder-01"
      (output, _) <-
        capture $
          processPath printFilePath handleDirectory path

      output `shouldContain` "File: 01_lorem_40000_words.txt"
      output `shouldContain` "Directory: test-folder-02"
  where
    printFilePath :: String -> IO ()
    printFilePath path = putStrLn $ "File: " ++ path

    printDirectoryPath :: String -> IO ()
    printDirectoryPath path = putStrLn $ "Directory: " ++ path

    handleDirectory :: String -> IO ()
    handleDirectory path = do
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left _ -> return ()
        Right entries ->
          mapM_
            ( \entry -> do
                isFile <- doesFileExist $ path </> entry
                if isFile
                  then printFilePath entry
                  else printDirectoryPath entry
            )
            entries
