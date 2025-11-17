module Main where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.ByteString (elem, readFile)
import Data.ByteString.Char8 (ByteString, count, empty, length, lines, pack, readFile, take, unpack)
import Data.Char (ord)
import Data.List (isPrefixOf)
import Data.Time (diffUTCTime, getCurrentTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import Text.Regex.TDFA (AllMatches, MatchLength, MatchOffset, getAllMatches, (=~))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  startTime <- getCurrentTime
  args <- getArgs
  case args of
    [pattern, path] -> do
      absolutePath <- makeAbsolute path
      --      let searchPattern = Data.ByteString.Char8.pack pattern
      processPath pattern absolutePath
    _ -> showUsageAndExit
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  putStrLn $ "\nDuration: " ++ show (realToFrac duration * 1000 :: Double) ++ " ms"

processPath :: String -> FilePath -> IO ()
processPath pattern path
  | isHidden path = return ()
  | otherwise = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then do
          isBinary <- isBinaryFile path
          unless isBinary $ handleFile pattern path
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

handleFile :: String -> FilePath -> IO ()
handleFile pattern path = do
  result <- try (Data.ByteString.Char8.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left err -> putStrLn $ "Error: Could not read file: " ++ path ++ " (" ++ show err ++ ")"
    Right content -> do
      let matches = getAllMatches (content =~ pattern :: AllMatches [] (MatchOffset, MatchLength))
      unless (null matches) $ do
        putStrLn $ "\n" ++ makeSafe path ++ " (" ++ show (Data.ByteString.Char8.length content) ++ " bytes)"
        mapM_
          ( \(offset, matchLength) -> do
              when (matchLength > 0) $ do
                let lineNum = getLineNumber content offset + 1
                let lineContent = getLineContent content lineNum
                putStrLn $ show lineNum ++ ": " ++ Data.ByteString.Char8.unpack lineContent
          )
          matches

handleDirectory :: String -> FilePath -> IO ()
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

-- TODO: extract file name from path
-- import System.FilePath (takeFileName)
-- let filename = takeFileName path
-- in filename /= "." && filename /= ".." && "." `isPrefixof` filename
isHidden :: FilePath -> Bool
isHidden path
  | path == "." = False
  | otherwise = "." `isPrefixOf` path

getLineNumber :: Data.ByteString.Char8.ByteString -> Int -> Int
getLineNumber content offset = Data.ByteString.Char8.count '\n' (Data.ByteString.Char8.take offset content)

getLineContent :: Data.ByteString.Char8.ByteString -> Int -> Data.ByteString.Char8.ByteString
getLineContent content lineNum =
  let allLines = Data.ByteString.Char8.lines content
   in if lineNum > 0 && lineNum <= Prelude.length allLines
        then allLines !! (lineNum - 1)
        else Data.ByteString.Char8.empty

isBinaryFile :: FilePath -> IO Bool
isBinaryFile path = do
  bs <- Data.ByteString.readFile path
  pure $ Data.ByteString.elem 0 bs -- 0 == NUL-Byte
