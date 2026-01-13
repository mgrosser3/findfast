module FindFast (findFast) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import qualified FindFast.ByteString as BS
import qualified FindFast.Glob as Glob
import FindFast.ProcessPath (processPathRecursive)
import qualified FindFast.RegEx as RegEx
import FindFast.Search (getLineContent, getLineNumber)
import FindFast.Utils (isHidden, makeSafe)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (userError)

-- import Text.Regex.TDFA (AllMatches, MatchLength, MatchOffset, getAllMatches, (=~))

findFast :: RegEx.Pattern -> FilePath -> IO ()
findFast pattern path = do
  absolutePath <- makeAbsolute path
  exists <- doesPathExist absolutePath
  if exists
    then processPathRecursive (handleFile pattern) absolutePath
    else throwIO $ userError $ "Path doesn't exist: " ++ absolutePath

-- TODO: Implement
findFastByGlob :: RegEx.Pattern -> Glob.CompiledPattern -> IO ()
findFastByGlob pattern glob = return ()

handleFile :: RegEx.Pattern -> FilePath -> IO ()
handleFile pattern filepath = do
  result <- try (BS.readFile filepath) :: IO (Either IOException BS.ByteString)
  case result of
    Left exception -> printError "Could not read file!" exception
    Right content -> printLinesWithMatches pattern filepath content

printLinesWithMatches :: RegEx.Pattern -> FilePath -> BS.ByteString -> IO ()
printLinesWithMatches filepath pattern content = do
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

printError :: String -> IOException -> IO ()
printError message exception =
  hPutStrLn stderr $
    "Error: " ++ message ++ "\n" ++ show exception
