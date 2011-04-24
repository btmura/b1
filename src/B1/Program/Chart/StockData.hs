module B1.Program.Chart.StockData
  ( StockData
  , StockPriceData(..)
  , newStockData
  , getStockPriceData
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Either
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Technicals.Stochastic
import B1.Program.Chart.Symbol

data StockData = StockData (MVar StockPriceData)

data StockPriceData = StockPriceData
  { startDate :: LocalTime
  , endDate :: LocalTime
  , pricesOrError :: Either [Price] String
  , dailyStochasticsOrError :: Either [Stochastic] String
  }

newStockData :: Symbol -> IO StockData
newStockData symbol = do
  priceDataMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate
    pricesOrError <- getGooglePrices startDate endDate symbol
    let dailyStochasticsOrError = either
            (\prices -> Left $ getStochastics 10 3 prices)
            Right
            pricesOrError
    putMVar priceDataMVar StockPriceData
      { startDate = startDate
      , endDate = endDate
      , pricesOrError = pricesOrError
      , dailyStochasticsOrError = dailyStochasticsOrError
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

