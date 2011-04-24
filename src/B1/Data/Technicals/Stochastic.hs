module B1.Data.Technicals.Stochastic
  ( Stochastic(..)
  , getStochastics
  , kFast
  , simpleMovingAverage
  , group
  ) where

import B1.Data.Price

data Stochastic = Stochastic Float Float
  deriving (Show, Eq)

getStochastics :: Int -> Int -> [Price] -> [Stochastic]
getStochastics kPeriods dPeriods prices = stochastics
  where
    priceGroups = group kPeriods prices
    kFastValues = map kFast priceGroups
    kSlowValues = simpleMovingAverage dPeriods kFastValues
    dSlowValues = simpleMovingAverage dPeriods kSlowValues

    maxLength = maximum $ map length [kSlowValues, dSlowValues]
    trimmedKSlowValues = take maxLength kSlowValues
    trimmedDSlowValues = take maxLength dSlowValues
    stochastics = map (\(k, d) -> Stochastic k d) $
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
    groups = group numPeriods list
    sums = map sum groups
    averages = map (/ (realToFrac numPeriods)) sums

group :: Int -> [a] -> [[a]]
group numPerGroup list
  | numPerGroup > length list = []
  | otherwise = first:rest
  where
    first = take numPerGroup list
    rest = group numPerGroup $ drop 1 list

