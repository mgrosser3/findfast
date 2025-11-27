module FindFast.Utils (isHidden, isBinaryFile) where

import Data.ByteString (elem, readFile)
import Data.List (isPrefixOf)

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
