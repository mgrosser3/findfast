module FindFast.Search (getLineContent) where

import Data.Char (ord)
import qualified FindFast.ByteString as BS

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
