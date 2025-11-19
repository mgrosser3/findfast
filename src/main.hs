module Main where

import Data.Time (diffUTCTime, getCurrentTime)
import FindFast.Path (processPath)
import System.Directory (makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  startTime <- getCurrentTime
  args <- getArgs
  case args of
    [pattern, path] -> do
      absolutePath <- makeAbsolute path
      processPath pattern absolutePath
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff FILEPATH"
  exitFailure
