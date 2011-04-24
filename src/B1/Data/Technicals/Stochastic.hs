module B1.Data.Technicals.Stochastic
  ( Stochastic(..)
  , getStochastics
  , kFast
  , simpleMovingAverage
  ) where

import B1.Data.List
import B1.Data.Price

data Stochastic = Stochastic 
  { k :: Float
  , d :: Float
  } deriving (Show, Eq)

getStochastics :: Int -> Int -> [Price] -> [Stochastic]
getStochastics kPeriods dPeriods prices = stochastics
  where
    priceGroups = groupElements kPeriods prices
    kFastValues = map kFast priceGroups
    kSlowValues = simpleMovingAverage dPeriods kFastValues
    dSlowValues = simpleMovingAverage dPeriods kSlowValues

    maxLength = maximum $ map length [kSlowValues, dSlowValues]
    trimmedKSlowValues = take maxLength kSlowValues
    trimmedDSlowValues = take maxLength dSlowValues
    stochastics = map (\(k, d) -> Stochastic { k = k, d = d}) $
        zip trimmedKSlowValues trimmedDSlowValues

kFast :: [Price] -> Float
kFast prices
  | denominator == 0 = 0
  | otherwise = numerator / denominator
  where
    currentClose = close $ head prices
    lowestLow = minimum $ map low prices
    highestHigh = maximum $ map high prices
    numerator = currentClose - lowestLow
    denominator = highestHigh - lowestLow

simpleMovingAverage :: Int -> [Float] -> [Float]
simpleMovingAverage numPeriods list = averages
  where
    groups = groupElements numPeriods list
    sums = map sum groups
    averages = map (/ (realToFrac numPeriods)) sums

