module FindFast.Glob (matches) where

matches :: String -> String -> Bool
matches [] [] = True
matches [] _ = False
matches ('*' : ps) s =
  matches ps s
    || case s of
      [] -> False
      (_ : xs) -> matches ('*' : ps) xs
matches ('?' : ps) (_ : xs) =
  matches ps xs
matches (p : ps) (s : xs)
  | p == s = matches ps xs
  | otherwise = False
matches _ _ = False
