-- | Module for getting stock prices from Google Finance.
module B1.Data.Price.Mock
  ( getMockPrices
  ) where

import Data.Time

import B1.Data.Price

-- | Get price information for testing.
getMockPrices :: Integral a => a -> [Price]
getMockPrices numPrices = take (fromIntegral numPrices) (repeat createPrice)

createPrice :: Price
createPrice = Price
  { startTime = LocalTime (fromGregorian 0 1 1) midnight
  , endTime = LocalTime (fromGregorian 0 1 1) midnight
  , open = 0
  , high = 0
  , low = 0
  , close = 0
  , volume = 0
  }

