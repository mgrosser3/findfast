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
import Text.Regex.TDFA (AllMatches, MatchLength, MatchOffset, getAllMatches, (=~))

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
handleFile pattern path = do
  result <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
  case result of
    Left exception ->
      System.IO.hPutStrLn stderr $
        "Error: Could not read file: "
          ++ path
          ++ " ("
          ++ show exception
          ++ ")"
    Right content -> do
      let matches = getAllMatches (content =~ pattern :: AllMatches [] (MatchOffset, MatchLength))
      unless (Prelude.null matches) $ do
        Prelude.putStrLn $ "\n" ++ makeSafe path ++ " (" ++ show (BS.length content) ++ " bytes)"
        mapM_
          ( \(offset, matchLength) -> do
              when (matchLength > 0) $ do
                let lineNum = getLineNumber content offset + 1
                let lineContent = getLineContent content lineNum
                Prelude.putStrLn $ show lineNum ++ ": " ++ BS.unpack lineContent
          )
          matches
