module Main where

import Data.Time (diffUTCTime, getCurrentTime)
import FindFast (findFast, findFastByGlob, findFastRecursive)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  startTime <- getCurrentTime
  args <- getArgs
  case args of
    [regex_pattern] -> findFast regex_pattern "."
    ["-r", regex_pattern] -> findFastRecursive regex_pattern "."
    [regex_pattern, path]
      | isGlobPattern path -> findFastByGlob regex_pattern path
      | otherwise -> findFast regex_pattern path
    ["-r", regex_pattern, path]
      | isGlobPattern path -> showUsageAndExit
      | otherwise -> findFastRecursive regex_pattern path
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff <regex-pattern> <path>"
  exitFailure

isGlobPattern :: String -> Bool
isGlobPattern input = any (`elem` input) ['*', '?', '[', '{']
