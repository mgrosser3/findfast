-- |
-- Abstract interface for file searches using glob patterns.
--
-- The concrete glob implementation is encapsulated, allowing it
-- to be transparently replaced without affecting the rest of the code.
module FindFast.Glob
  ( Matchable (match),
    Pattern,
    CompiledPattern,
    pattern,
    compile,
  )
where

import qualified System.FilePath.Glob as Internal

newtype Pattern = Pattern String

newtype CompiledPattern = CompiledPattern Internal.Pattern

-- Type Class
class Matchable pattern where
  match :: pattern -> FilePath -> Bool

-- |
-- Matches the given textual representation of glob pattern against the given
-- FilePath, returning True if the pattern matches and False otherwise.
instance Matchable Pattern where
  -- NOTE: second parameter can be omitted due to eta reduction
  match (Pattern pattern) = Internal.match (Internal.compile pattern)

instance Matchable CompiledPattern where
  -- NOTE: second parameter can be omitted due to eta reduction
  match (CompiledPattern pattern) = Internal.match pattern

-- |
-- Compiles a glob pattern from its textual representation into a
-- CompiledPattern object.
compile :: String -> CompiledPattern
compile pattern = CompiledPattern $ Internal.compile pattern

-- |
-- Constructor function for Pattern object.
pattern :: String -> Pattern
-- NOTE: parameter can be omitted due to eta reduction
pattern = Pattern
