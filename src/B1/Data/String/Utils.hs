module B1.Data.String.Utils
  ( split
  ) where

split :: String -> Char -> [String]
split [] _ = []
split string delimiter = first : rest
  where
    (first, restWithDelimiter) = break (== delimiter) string
    rest = case restWithDelimiter of
      [] -> []
      (_:restWithoutDelimiter) -> split restWithoutDelimiter delimiter

