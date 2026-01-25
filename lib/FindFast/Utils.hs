module FindFast.Utils
  ( isHidden,
    isBinaryFile,
    makeSafe,
    printError,
  )
where

import Control.Exception (IOException)
import Data.ByteString (elem, readFile)
import Data.Char (isAscii)
import Data.List (isPrefixOf)
import System.IO (hPutStrLn, stderr)

isBinaryFile :: FilePath -> IO Bool
isBinaryFile path = do
  bs <- Data.ByteString.readFile path
  pure $ Data.ByteString.elem 0 bs -- 0 == NUL-Byte

-- FIXME: already in Path module
-- TODO: extract file name from path
-- import System.FilePath (takeFileName)
-- let filename = takeFileName path
-- in filename /= "." && filename /= ".." && "." `isPrefixof` filename
isHidden :: FilePath -> Bool
isHidden path
  | path == "." = False
  | otherwise = "." `isPrefixOf` path

makeSafe :: String -> String
makeSafe = concatMap (\char -> if isAscii char then [char] else yellow "?")
  where
    yellow :: String -> String
    yellow text = "\ESC[33m" ++ text ++ "\ESC[0m"

printError :: String -> IOException -> IO ()
printError message exception =
  hPutStrLn stderr $
    "Error: " ++ message ++ "\n" ++ show exception
