module FindFast.Search where

import Data.ByteString.Char8 (ByteString, count, empty, lines, take)
import Data.Char (ord)

makeSafe :: String -> String
makeSafe = map (\c -> if ord c > 127 then '?' else c)

getLineNumber :: Data.ByteString.Char8.ByteString -> Int -> Int
getLineNumber content offset = Data.ByteString.Char8.count '\n' (Data.ByteString.Char8.take offset content)

getLineContent :: Data.ByteString.Char8.ByteString -> Int -> Data.ByteString.Char8.ByteString
getLineContent content lineNum =
  let allLines = Data.ByteString.Char8.lines content
   in if lineNum > 0 && lineNum <= Prelude.length allLines
        then allLines !! (lineNum - 1)
        else Data.ByteString.Char8.empty
