module FindFast (findFast, findFastRecursive, findFastGlob) where

import Control.Concurrent.Async
import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import qualified FindFast.ByteString as BS
import qualified FindFast.Glob as Glob
import FindFast.ProcessPath (processPath, processPathGlob, processPathRecursive)
import qualified FindFast.RegEx as RegEx
import FindFast.Utils (isHidden, makeSafe, printError)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (userError)

-- | Search files using a regex pattern.
--
-- Searches through files in the given directory (non-recursive) or single file
-- and prints matches with line numbers.
--
-- Example:
--
-- >>> findFast "error|warning" "./logs"
-- Match in: ./logs/app.log (1024 bytes)
--   Line 5: Error occurred
findFast :: RegEx.Pattern -> FilePath -> IO ()
findFast pattern path = do
  absolutePath <- makeAbsolute path
  exists <- doesPathExist path
  if exists
    then processPath (handleFile pattern) (handleDir pattern) absolutePath
    else throwIO $ userError $ "Path doesn't exist: " ++ absolutePath
  where
    handleDir :: RegEx.Pattern -> FilePath -> IO ()
    handleDir pattern path = do
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left exception -> printError "Could not read directory!" exception
        Right entries ->
          mapM_
            ( \entry ->
                unless (isHidden entry) $
                  processPath (handleFile pattern) (\_ -> return ()) entry
            )
            entries

-- | Search files recursively using a regex pattern.
--
-- Like 'findFast', but recursively searches through all subdirectories.
-- Searches through all files in the given directory tree (or single file)
-- and prints matches with line numbers.
--
-- Example:
--
-- >>> findFastRecursive "error|warning" "./logs"
-- Match in: ./logs/app.log (1024 bytes)
--   Line 5: Error occurred
-- Match in: ./logs/2024/january.log (512 bytes)
--   Line 12: Warning: Low memory
findFastRecursive :: RegEx.Pattern -> FilePath -> IO ()
findFastRecursive pattern path = do
  absolutePath <- makeAbsolute path
  exists <- doesPathExist absolutePath
  if exists
    then processPathRecursive (handleFile pattern) absolutePath
    else throwIO $ userError $ "Path doesn't exist: " ++ absolutePath

-- | Search files matching a glob pattern and apply the given regular
--   expression to each file.
--
--   The glob pattern is expanded to a list of file paths using
--   'Glob.glob'. Each matching file is then processed concurrently
--   with 'mapConcurrently_'.
--
--   This function performs IO and returns no result; it is intended
--   for its side effects (e.g. printing matches).
findFastGlob :: RegEx.Pattern -> String -> IO ()
findFastGlob regexPattern globPattern =
  --  NOTE: The result of 'Glob.glob' is passed to 'mapConcurrently_' using
  --  the bind operator '(>>=)'.
  Glob.glob globPattern >>= mapConcurrently_ (handleFile regexPattern)

--
--
--
--

-- | Internal handler to search a single file for pattern matches.
handleFile :: RegEx.Pattern -> FilePath -> IO ()
handleFile pattern filepath = do
  result <- try (BS.readFile filepath) :: IO (Either IOException BS.ByteString)
  case result of
    Left exception -> printError "Could not read file!" exception
    Right content -> printLinesWithMatches pattern filepath content

-- | Internal helper function to prints matching lines with their line numbers.
printLinesWithMatches :: RegEx.Pattern -> FilePath -> BS.ByteString -> IO ()
printLinesWithMatches pattern filepath content = do
  let matches = RegEx.getAllMatches pattern content
  unless (null matches) $ putStrLn $ "\n\x1b[32m" ++ makeSafe filepath ++ "\x1b[0m (" ++ show (BS.length content) ++ " bytes)"
  mapM_
    ( \(offset, length) -> do
        when (length > 0) $ do
          let (lineNum, lineOffset, lineContent) = getLineContent content offset
          putStrLn $ show lineNum ++ ": " ++ makeSafe (BS.unpack (highlightMatches (offset - lineOffset) length lineContent))
    )
    matches
  where
    -- Internal helper function to highlight the matched parts in the line
    highlightMatches :: Int -> Int -> BS.ByteString -> BS.ByteString
    highlightMatches offset length line =
      let (before, rest) = BS.splitAt offset line
          (middle, after) = BS.splitAt length rest
       in before
            <> BS.pack "\x1b[31m"
            <> middle
            <> BS.pack "\x1b[0m"
            <> after

-- | Internal helper function to get line information at the given byte offset.
getLineContent :: BS.ByteString -> Int -> (Int, Int, BS.ByteString)
getLineContent content offset =
  let -- all before the offset
      (before, _) = BS.splitAt offset content

      -- line number (starts with 1)
      lineNum = BS.count '\n' before + 1

      -- global offset of the line
      lineStart = case BS.elemIndexEnd '\n' before of
        Nothing -> 0
        Just i -> i + 1

      -- line content
      contentFromLineStart = BS.drop lineStart content
      lineContent = BS.takeWhile (/= '\n') contentFromLineStart
   in (lineNum, lineStart, lineContent)
