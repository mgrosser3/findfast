module FindFast (findFast, findFastRecursive, findFastGlob) where

import Control.Concurrent.Async
import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import qualified FindFast.ByteString as BS
import qualified FindFast.Glob as Glob
import FindFast.ProcessPath (processPath, processPathGlob, processPathRecursive)
import qualified FindFast.RegEx as RegEx
import FindFast.Search (getLineContent)
import FindFast.Utils (isHidden, makeSafe, printError)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
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

-- | Search files matching a glob pattern using a regex pattern.
--
-- Like 'findFast', but uses a glob pattern to filter which files to search.
-- Only files matching the glob pattern will be searched for the regex pattern.
--
-- Example:
--
-- >>> pattern <- "**/*.log"
-- >>> findFastByGlob "error|warning" pattern
-- Match in: ./logs/app.log (1024 bytes)
--   Line 5: Error occurred
-- Match in: ./logs/system.log (2048 bytes)
--   Line 23: Warning: Disk space low
findFastByGlob :: RegEx.Pattern -> String -> IO ()
-- TODO: remove byGlob from name
findFastByGlob pattern glob = do
  let (path, globPattern) = Glob.commonDirectory (Glob.compile glob)
  absolutePath <- makeAbsolute path
  putStrLn "\n\n"
  putStrLn $ "Search in Path: " ++ absolutePath
  putStrLn $ "With Pattern: " ++ Glob.decompile globPattern
  putStrLn "\n\n"
  processPathGlob handleFileGlob (handleDirGlob $ Glob.pattern $ Glob.decompile globPattern) absolutePath
  return ()
  where
    handleFileGlob :: FilePath -> IO ()
    handleFileGlob path = do
      putStrLn $ "File: " ++ path
      return ()
    --
    handleDirGlob :: Glob.Pattern -> FilePath -> IO ()
    handleDirGlob globPattern path = do
      putStrLn $ "Dir: " ++ path
      result <- try (listDirectory path) :: IO (Either IOException [String])
      case result of
        Left exception -> printError "Could not read directory!" exception
        Right entries ->
          mapM_
            ( \entry -> do
                when (Glob.match globPattern entry) $
                  let (_, next) = break (== '/') $ Glob.toString globPattern
                   in processPathGlob handleFileGlob (handleDirGlob $ Glob.pattern next) (path </> entry)
            )
            entries

-- | Find file with glob file pattern
findFastGlob :: RegEx.Pattern -> String -> IO ()
findFastGlob regexPattern globPattern =
  -- TODO: try mapConcurrently_
  Glob.glob globPattern >>= mapConcurrently_ (handleFile regexPattern)

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
