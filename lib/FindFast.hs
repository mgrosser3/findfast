module FindFast (findFast) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString.Char8 as BC
import FindFast.ProcessPath (processPath)
import FindFast.Search (getLineContent, getLineNumber)
import FindFast.Utils (isHidden, makeSafe)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (userError)
import Text.Regex.TDFA (AllMatches, MatchLength, MatchOffset, getAllMatches, (=~))

handleFile :: String -> FilePath -> IO ()
handleFile pattern path = do
  result <- try (BC.readFile path) :: IO (Either IOException ByteString)
  case result of
    Left err -> System.IO.hPutStrLn stderr $ "Error: Could not read file: " ++ path ++ " (" ++ show err ++ ")"
    Right content -> do
      let matches = getAllMatches (content =~ pattern :: AllMatches [] (MatchOffset, MatchLength))
      unless (Prelude.null matches) $ do
        Prelude.putStrLn $ "\n" ++ makeSafe path ++ " (" ++ show (BC.length content) ++ " bytes)"
        mapM_
          ( \(offset, matchLength) -> do
              when (matchLength > 0) $ do
                let lineNum = getLineNumber content offset + 1
                let lineContent = getLineContent content lineNum
                Prelude.putStrLn $ show lineNum ++ ": " ++ BC.unpack lineContent
          )
          matches

handleDirectory :: String -> FilePath -> IO ()
handleDirectory pattern path = do
  result <- try $ listDirectory path :: IO (Either IOException [String])
  case result of
    Left err ->
      System.IO.hPutStrLn stderr $
        "Error: Could not read directory: "
          ++ path
          ++ " ("
          ++ show err
          ++ ")"
    Right entries -> do
      mapM_
        ( \entry -> do
            unless (isHidden entry) $
              processPath (handleFile pattern) (handleDirectory pattern) $
                path </> entry
        )
        entries

findFast :: String -> FilePath -> IO ()
findFast pattern path = do
  absolutePath <- makeAbsolute path
  exists <- doesPathExist absolutePath
  if exists
    then processPath (handleFile pattern) (handleDirectory pattern) absolutePath
    else throwIO $ userError $ "Path doesn't exist: " ++ absolutePath
