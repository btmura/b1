module B1.Data.List
  ( groupElements
  ) where

groupElements :: Int -> [a] -> [[a]]
groupElements numPerGroup list
  | numPerGroup > length list = []
  | otherwise = first:rest
  where
    first = take numPerGroup list
    rest = groupElements numPerGroup $ drop 1 list

