-- | Module for getting stock prices.
module B1.Data.Price
  ( Price (..)
  ) where

import Data.Time

-- | Price information during some time interval.
data Price = Price
  { startTime :: LocalTime -- ^ Start time of the trading period
  , endTime :: LocalTime -- ^ End time of the trading period
  , open :: Float -- ^ Opening price of the trading period
  , high :: Float -- ^ Highest price during the trading period
  , low :: Float -- ^ Lowest price during the trading period
  , close :: Float -- ^ Closing price of the trading period.
  , volume :: Int -- ^ Volume of the trading period.
  } deriving (Show)

