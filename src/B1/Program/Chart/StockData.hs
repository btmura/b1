module B1.Program.Chart.StockData
  ( StockData
  , StockPriceData(..)
  , newStockData
  , getStockPriceData
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO

import B1.Data.Price
import B1.Data.Price.Google
import B1.Program.Chart.Symbol

data StockData = StockData (MVar StockPriceData)

data StockPriceData = StockPriceData
  { startDate :: LocalTime
  , endDate :: LocalTime
  , prices :: Prices
  }

newStockData :: Symbol -> IO StockData
newStockData symbol = do
  priceDataMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate
    prices <- getGooglePrices startDate endDate symbol
    putMVar priceDataMVar $ StockPriceData
      { startDate = startDate
      , endDate = endDate
      , prices = prices
      }
  return $ StockData priceDataMVar

getStartDate :: IO LocalTime
getStartDate = do
  endDate <- getEndDate
  let yearAgo = addGregorianYearsClip (-1) (localDay endDate)
  return endDate
    { localDay = yearAgo
    , localTimeOfDay = midnight
    }

getEndDate :: IO LocalTime
getEndDate = do
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  let localTime = utcToLocalTime timeZone time 
  return $ localTime { localTimeOfDay = midnight }

getStockPriceData :: StockData -> IO (Maybe StockPriceData)
getStockPriceData (StockData pricesMVar) = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

