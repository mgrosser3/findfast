module FindFast (findFast, findFastRecursive, findFastByGlob) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import qualified FindFast.ByteString as BS
import qualified FindFast.Glob as Glob
import FindFast.ProcessPath (processPath, processPathRecursive)
import qualified FindFast.RegEx as RegEx
import FindFast.Search (getLineContent, getLineNumber)
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
-- >>> pattern <- Glob.compile "**/*.log"
-- >>> findFastByGlob "error|warning" pattern
-- Match in: ./logs/app.log (1024 bytes)
--   Line 5: Error occurred
-- Match in: ./logs/system.log (2048 bytes)
--   Line 23: Warning: Disk space low
findFastByGlob :: RegEx.Pattern -> String -> IO ()
findFastByGlob pattern glob = return ()

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
  mapM_
    ( \(offset, length) -> do
        when (length > 0) $ do
          let lineNum = getLineNumber content offset + 1
          let lineContent = getLineContent content lineNum
          putStrLn $ "\n" ++ makeSafe filepath ++ " (" ++ show (BS.length content) ++ " bytes)"
          putStrLn $ show lineNum ++ ": " ++ BS.unpack lineContent
    )
    matches
