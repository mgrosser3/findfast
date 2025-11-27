module FindFast (findFast) where

import Control.Exception (IOException, throwIO, try)
import Control.Monad (unless)
import FindFast.ProcessPath (processPath)
import FindFast.Utils (isHidden)
import System.Directory (doesPathExist, listDirectory, makeAbsolute)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (userError)

handleFile :: String -> FilePath -> IO ()
handleFile pattern path = do
  hPutStrLn stderr "Not Implemented"

handleDirectory :: String -> FilePath -> IO ()
handleDirectory pattern path = do
  result <- try $ listDirectory path :: IO (Either IOException [String])
  case result of
    Left err ->
      hPutStrLn stderr $
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
