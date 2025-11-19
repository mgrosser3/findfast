module FindFast.Path where

import Control.Exception (IOException, try)
import Control.Monad (unless, when)
import Data.ByteString (elem, readFile)
import Data.ByteString.Char8 (ByteString, length, readFile, unpack)
import Data.List (isPrefixOf)
import FindFast.Search
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Text.Regex.TDFA (AllMatches, MatchLength, MatchOffset, getAllMatches, (=~))

processPath :: (FilePath -> IO ()) -> (FilePath -> IO ()) -> FilePath -> IO ()
processPath handleFile handleDirectory path
  | isHidden path = return ()
  | otherwise = do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path

      if isFile
        then do
          isBinary <- isBinaryFile path
          unless isBinary $ handleFile path
        else
          if isDir
            then handleDirectory path
            else showPathNotFoundError path

-- handleDirectory :: String -> FilePath -> IO ()
-- handleDirectory pattern path = do
--   result <- try $ listDirectory path :: IO (Either IOException [String])
--   case result of
--     Left err -> putStrLn $ "Error: Could not read directory: " ++ path ++ " (" ++ show err ++ ")"
--     Right subpathes -> do
--       mapM_
--         ( \subpath -> do
--             unless (isHidden subpath) $
--               processPath pattern $
--                 path </> subpath
--         )
--         subpathes
--
-- handleFile :: String -> FilePath -> IO ()
-- handleFile pattern path = do
--   result <- try (Data.ByteString.Char8.readFile path) :: IO (Either IOException ByteString)
--   case result of
--     Left err -> putStrLn $ "Error: Could not read file: " ++ path ++ " (" ++ show err ++ ")"
--     Right content -> do
--       let matches = getAllMatches (content =~ pattern :: AllMatches [] (MatchOffset, MatchLength))
--       unless (null matches) $ do
--         putStrLn $ "\n" ++ makeSafe path ++ " (" ++ show (Data.ByteString.Char8.length content) ++ " bytes)"
--         mapM_
--           ( \(offset, matchLength) -> do
--               when (matchLength > 0) $ do
--                 let lineNum = getLineNumber content offset + 1
--                 let lineContent = getLineContent content lineNum
--                 putStrLn $ show lineNum ++ ": " ++ Data.ByteString.Char8.unpack lineContent
--           )
--           matches
--
showPathNotFoundError :: FilePath -> IO ()
showPathNotFoundError path = hPutStrLn stderr $ "Error: Path doesn't exist: " ++ path

-- FIXME: already in Path module
-- TODO: extract file name from path
-- import System.FilePath (takeFileName)
-- let filename = takeFileName path
-- in filename /= "." && filename /= ".." && "." `isPrefixof` filename
isHidden :: FilePath -> Bool
isHidden path
  | path == "." = False
  | otherwise = "." `isPrefixOf` path

isBinaryFile :: FilePath -> IO Bool
isBinaryFile path = do
  bs <- Data.ByteString.readFile path
  pure $ Data.ByteString.elem 0 bs -- 0 == NUL-Byte
