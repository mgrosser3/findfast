{-# LANGUAGE NoImplicitPrelude #-}

module FindFast.ByteString (ByteString, readFile, length, unpack) where

--
--    Internal ByteString Module
--
import qualified Data.ByteString.Char8 as BC
--
--    Prelude Module
--
import Prelude (Char, FilePath, IO, Int)

type ByteString = BC.ByteString

readFile :: FilePath -> IO ByteString
readFile = BC.readFile

length :: ByteString -> Int
length = BC.length

unpack :: ByteString -> [Char]
unpack = BC.unpack
