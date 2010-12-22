module B1.Data.String.Utils
  ( split
  ) where

split :: Char -> String -> [String]
split _ [] = []
split delimiter string = first : rest
  where
    (first, restWithDelimiter) = break (== delimiter) string
    rest = case restWithDelimiter of
      [] -> []
      (_:restWithoutDelimiter) -> split delimiter restWithoutDelimiter

