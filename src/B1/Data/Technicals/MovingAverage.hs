module B1.Data.Technicals.MovingAverage
  ( MovingAverage(..)
  , getMovingAverage
  ) where

import B1.Data.List
import B1.Data.Price

type MovingAverage = Float

getMovingAverage :: Int -> [Price] -> [MovingAverage]
getMovingAverage numDays prices =
  map average $ groupElements numDays $ map close prices

average :: [Float] -> Float
average values = realToFrac (sum values) / realToFrac (length values)

