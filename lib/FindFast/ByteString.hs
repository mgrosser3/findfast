{-# LANGUAGE NoImplicitPrelude #-}

module FindFast.ByteString
  ( ByteString,
    readFile,
    length,
    unpack,
    pack,
    splitAt,
    count,
    elemIndexEnd,
    drop,
    takeWhile,
  )
where

--
--    Internal ByteString Module
--
import qualified Data.ByteString.Char8 as BC
--
--    Prelude Module
--
import Prelude (Bool, Char, FilePath, IO, Int, Maybe)

type ByteString = BC.ByteString

readFile :: FilePath -> IO ByteString
readFile = BC.readFile

length :: ByteString -> Int
length = BC.length

pack :: [Char] -> ByteString
pack = BC.pack

unpack :: ByteString -> [Char]
unpack = BC.unpack

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt = BC.splitAt

count :: Char -> ByteString -> Int
count = BC.count

elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = BC.elemIndexEnd

drop :: Int -> ByteString -> ByteString
drop = BC.drop

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile = BC.takeWhile
