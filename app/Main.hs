module Main where

import Data.Time (diffUTCTime, getCurrentTime)
import FindFast (findFast, findFastGlob, findFastRecursive)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)

showUsage :: IO ()
showUsage =
  putStrLn $
    unlines
      [ "",
        "  FindFast 1.0.0",
        "  Search for Regex Pattern in Files.",
        "  ===================================================",
        "",
        "  Usage: ff [OPTION]... PATTERN PATH",
        "     or: ff [OPTION]... PATTERN GLOB-PATTERN",
        "",
        "  Options:",
        "    -r, --recursive    Search directories recursively",
        "    -h, --help         Display this help and exit",
        "",
        "  Examples:",
        "    ff main Main.hs",
        "    ff -r TODO src/",
        "    ff error \"**/*.log\"",
        ""
      ]

main :: IO ()
main = do
  --
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  --
  startTime <- getCurrentTime
  --
  args <- getArgs
  case args of
    ["-h"] -> showUsage >> exitSuccess
    ["--help"] -> showUsage >> exitSuccess
    --
    [regex_pattern] -> findFast regex_pattern "."
    ["-r", regex_pattern] -> findFastRecursive regex_pattern "."
    ["--recursive", regex_pattern] -> findFastRecursive regex_pattern "."
    --
    [regex_pattern, path]
      | isGlobPattern path -> findFastGlob regex_pattern path
      | otherwise -> findFast regex_pattern path
    --
    ["-r", regex_pattern, path]
      | isGlobPattern path -> showUsage >> exitFailure
      | otherwise -> findFastRecursive regex_pattern path
    --
    _ -> showUsage >> exitFailure
  --
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"
  --
  exitSuccess

isGlobPattern :: String -> Bool
isGlobPattern input = any (`elem` input) ['*', '?', '[', '{']
