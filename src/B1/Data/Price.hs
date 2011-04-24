-- | Module for getting stock prices.
module B1.Data.Price
  ( Price (..)
  , getPriceChange
  , getWeeklyPrices
  ) where

import Data.List
import Data.Time
import Data.Time.Calendar.WeekDate

-- | Price information during some time interval.
data Price = Price
  { startTime :: LocalTime -- ^ Start time of the trading period
  , endTime :: LocalTime -- ^ End time of the trading period
  , open :: Float -- ^ Opening price of the trading period
  , high :: Float -- ^ Highest price during the trading period
  , low :: Float -- ^ Lowest price during the trading period
  , close :: Float -- ^ Closing price of the trading period.
  , volume :: Int -- ^ Volume of the trading period.
  } deriving (Show, Eq)

getPriceChange :: [Price] -> Int -> Float
getPriceChange prices index 
  | index + 1 < length prices = change
  | otherwise = 0
  where
    currClose = close $ prices !! index
    prevClose = close $ prices !! (index + 1)
    change = currClose - prevClose

getWeeklyPrices :: [Price] -> [Price]
getWeeklyPrices dailyPrices =
  map flattenWeeklyPriceGroup $ getWeeklyPriceGroups dailyPrices

getWeeklyPriceGroups :: [Price] -> [[Price]]
getWeeklyPriceGroups dailyPrices = groupBy sameWeekNumber dailyPrices

sameWeekNumber :: Price -> Price -> Bool
sameWeekNumber price otherPrice =
  let week = getWeekNumber price
      otherWeek = getWeekNumber otherPrice
  in week == otherWeek

getWeekNumber :: Price -> Int
getWeekNumber price = weekNumber
  where
    day = localDay $ endTime price
    (_, weekNumber, _) = toWeekDate day

flattenWeeklyPriceGroup :: [Price] -> Price
flattenWeeklyPriceGroup prices =
  Price
    { startTime = flatStartTime
    , endTime = flatEndTime
    , open = flatOpen
    , high = flatHigh
    , low = flatLow
    , close = flatClose
    , volume = flatVolume
    }
  where
    flatStartTime = startTime $ last prices
    flatEndTime = endTime $ head prices
    flatOpen = open $ last prices
    flatHigh = maximum $ map high prices
    flatLow = minimum $ map low prices
    flatClose = close $ head prices
    flatVolume = sum $ map volume prices

