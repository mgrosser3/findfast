module FindFast.RegEx (Pattern) where

import FindFast.ByteString (ByteString)
--
--    Internal RegEx Module
--
import Text.Regex.TDFA ((=~))
import qualified Text.Regex.TDFA as TDFA

type Pattern = String

type Lines = [(Int, String)]

type MatchOffset = Int

type MatchLength = Int

getAllMatches :: ByteString -> Pattern -> [(MatchOffset, MatchLength)]
getAllMatches _ [] = []
getAllMatches content pattern = TDFA.getAllMatches (content =~ pattern :: TDFA.AllMatches [] (TDFA.MatchOffset, TDFA.MatchLength))
