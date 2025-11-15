module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  -- getArgs provides IO [String]
  args <- getArgs
  case args of
    [filepath] -> do
      -- readFile provides IO String
      content <- readFile filepath
      putStrLn content
    _ -> do
      hPutStrLn stderr "Usage: ff FILEPATH"
      exitFailure
