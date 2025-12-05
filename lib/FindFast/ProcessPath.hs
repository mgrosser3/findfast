-- | This module provides a framework for working with file paths.
-- It contains a number of functions designed to help you work with
-- files and directories.
module FindFast.ProcessPath (processPath) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless)
import FindFast.Search
import FindFast.Utils (isBinaryFile, isHidden)
import System.Directory (doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- | The function `processPath` offers the option of passing your own functions
-- as handlers, which are applied to file paths and directory paths.
--
--   @
--   processPath printFilePath printDirectoryPath "."
--      where
--          printFilePath :: String -> IO ()
--          printFilePath path = putStrLn $ "File: " ++ path
--          printDirectoryPath :: String -> IO ()
--          printDirectoryPath path = putStrLn $ "Directory: " ++ path
--   @
processPath :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
processPath handleFile handleDirectory path
  | isHidden path = return ()
  | otherwise = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then do
          isBinary <- isBinaryFile path
          unless isBinary $ handleFile path
        else
          if isDir
            then handleDirectory path
            else showPathNotFoundError path

showPathNotFoundError :: FilePath -> IO ()
showPathNotFoundError path = hPutStrLn stderr $ "Error: Path doesn't exist: " ++ path

-- TODO: Version A of a recursive process path solution based on currying
processPathRecursive :: (FilePath -> IO ()) -> FilePath -> IO ()
processPathRecursive handleFile path = processPath handleFile (handleDirectory handleFile) path
  where
    handleDirectory :: (FilePath -> IO ()) -> FilePath -> IO ()
    handleDirectory handleFile path = do
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left _ -> return ()
        Right entries ->
          mapM_
            ( \entry ->
                processPathRecursive handleFile (path </> entry)
            )
            entries

-- TODO: Version B of a recursive process path solution
processPathRecursiveV2 :: (FilePath -> IO ()) -> FilePath -> IO ()
processPathRecursiveV2 handleFile = process
  where
    process = processPath handleFile recurse

    recurse path = do
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left _ -> return ()
        Right entries -> mapM_ (\entry -> process (path </> entry)) entries
