-- | This module provides a framework for working with file paths.
-- It contains a number of functions designed to help you work with
-- files and directories.
module FindFast.ProcessPath (processPath, processPathRecursive) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless)
import FindFast.Glob (match)
import FindFast.Search
import FindFast.Utils (isBinaryFile, isHidden, printError)
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
            else hPutStrLn stderr $ "Path does not exist!" ++ path

-- | Recursively traverses a given file path and applies a function to
--   every file encountered.
--
--   The function takes a file handler of type @(FilePath -> IO ())@
--   and a starting path. The starting path may refer to either a file
--   or a directory. If it is a directory, all subdirectories will be
--   traversed recursively.
--
--   For every file discovered during the traversal, the provided
--   handler function is executed. The entire operation is performed
--   within the 'IO' monad.
--
--   Example:
--
--   @
--   processPathRecursive printFilePath "."
--      where
--          printFilePath :: String -> IO ()
--          printFilePath path = putStrLn $ "File: " ++ path
--   @
processPathRecursive :: (FilePath -> IO ()) -> FilePath -> IO ()
processPathRecursive handleFile = process
  where
    -- process is a function (see currying)
    -- handleFile is available within this scope via closure
    process = processPath handleFile handleDirectory

    handleDirectory :: FilePath -> IO ()
    handleDirectory path = do
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left exception -> printError "Could not read directory!" exception
        Right entries ->
          mapM_ (\entry -> unless (isHidden entry) $ process (path </> entry)) entries
