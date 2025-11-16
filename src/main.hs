module Main where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.ByteString.Char8 (ByteString, length, pack, readFile)
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import Text.Regex.TDFA ((=~))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  startTime <- getCurrentTime
  args <- getArgs
  case args of
    [pattern, path] -> do
      absolutePath <- makeAbsolute path
      let searchPattern = Data.ByteString.Char8.pack pattern
      processPath searchPattern absolutePath
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

processPath :: Data.ByteString.Char8.ByteString -> FilePath -> IO ()
processPath pattern path
  | isHidden path = return ()
  | otherwise = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then handleFile pattern path
        else
          if isDir
            then handleDirectory pattern path
            else showPathNotFoundError path

showUsageAndExit :: IO ()
showUsageAndExit = do
  hPutStrLn stderr "Usage: ff FILEPATH"
  exitFailure

makeSafe :: String -> String
makeSafe = map (\c -> if ord c > 127 then '?' else c)

handleFile :: Data.ByteString.Char8.ByteString -> FilePath -> IO ()
handleFile pattern path = do
  result <- try (Data.ByteString.Char8.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left err -> putStrLn $ "Error: Could not read file: " ++ path ++ " (" ++ show err ++ ")"
    Right content -> do
      when (content =~ pattern :: Bool) $
        putStrLn $
          "Find in:" ++ makeSafe path ++ " (" ++ show (Data.ByteString.Char8.length content) ++ " bytes)"

handleDirectory :: Data.ByteString.Char8.ByteString -> FilePath -> IO ()
handleDirectory pattern path = do
  result <- try $ listDirectory path :: IO (Either IOException [String])
  case result of
    Left err -> putStrLn $ "Error: Could not read directory: " ++ path ++ " (" ++ show err ++ ")"
    Right subpathes -> do
      mapM_
        ( \subpath -> do
            unless (isHidden subpath) $
              processPath pattern $
                path </> subpath
        )
        subpathes

showPathNotFoundError :: FilePath -> IO ()
showPathNotFoundError path = hPutStrLn stderr $ "Error: Path doesn't exist: " ++ path

isHidden :: FilePath -> Bool
isHidden path
  | path == "." = False
  | otherwise = "." `isPrefixOf` path
