module Main where

import Control.Monad (when)
import Data.Time (diffUTCTime, getCurrentTime)
import FindFast (findFast, findFastRecursive)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

main :: IO ()
main = do
  startTime <- getCurrentTime

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  args <- getArgs
  case args of
    [regex_pattern] -> findFast regex_pattern "."
    [regex_pattern, path] -> findFast regex_pattern path
    ["-r", regex_pattern, path] -> findFastRecursive regex_pattern path
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff <regex-pattern> <path>"
  exitFailure
