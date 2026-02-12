-- | Internal module that encapsulates all RegEx-related functionality
-- for the FindFast project. It prevents third-party dependencies from leaking
-- int the rest of the codebase and simplifies testing and future maintenance.
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
