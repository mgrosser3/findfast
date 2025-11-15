module Main where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  startTime <- getCurrentTime
  args <- getArgs
  case args of
    [path] -> do
      absolutePath <- makeAbsolute path
      processPath absolutePath
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

processPath :: FilePath -> IO ()
processPath path
  | isHidden path = return ()
  | otherwise = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then handleFile path
        else
          if isDir
            then handleDirectory path
            else showPathNotFoundError path

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff FILEPATH"
  exitFailure

handleFile :: FilePath -> IO ()
handleFile path = putStrLn $ "Search in File: " ++ path

handleDirectory :: FilePath -> IO ()
handleDirectory path = do
  subpathes <- listDirectory path
  mapM_
    ( \subpath -> do
        unless (isHidden subpath) $
          processPath $
            path </> subpath
    )
    subpathes

showPathNotFoundError :: FilePath -> IO ()
showPathNotFoundError path = hPutStrLn stderr $ "Error: Path doesn't exist: " ++ path

isHidden :: FilePath -> Bool
isHidden path
  | path == "." = False
  | otherwise = "." `isPrefixOf` path
