module FindFast.ProcessPath (processPath) where

import Control.Monad (unless)
import FindFast.Search
import FindFast.Utils (isBinaryFile, isHidden)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.IO (hPutStrLn, stderr)

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
