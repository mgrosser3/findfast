module FindFast.RegEx (Pattern, MatchOffset, MatchLength, getAllMatches) where

import FindFast.ByteString (ByteString)
import Text.Regex.TDFA ((=~))
import qualified Text.Regex.TDFA as TDFA

type Pattern = String

type MatchOffset = Int

type MatchLength = Int

getAllMatches :: Pattern -> ByteString -> [(MatchOffset, MatchLength)]
getAllMatches [] _ = []
getAllMatches pattern content =
  TDFA.getAllMatches (content =~ pattern :: TDFA.AllMatches [] (TDFA.MatchOffset, TDFA.MatchLength))
