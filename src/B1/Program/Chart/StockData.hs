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

data StockData = StockData (MVar (Either StockPriceData String))

data StockPriceData = StockPriceData
  { prices :: [Price]
  , stochastics :: [Stochastic]
  , weeklyPrices :: [Price]
  , weeklyStochastics :: [Stochastic]
  }

newStockData :: Symbol -> IO StockData
newStockData symbol = do
  priceDataMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate
    pricesOrError <- getGooglePrices startDate endDate symbol
    putMVar priceDataMVar $ either
        (\prices ->
            let weeklyPrices = getWeeklyPrices prices
                stochasticsFunction = getStochastics 10 3
            in Left StockPriceData
              { prices = prices
              , stochastics = stochasticsFunction prices
              , weeklyPrices =  weeklyPrices
              , weeklyStochastics = stochasticsFunction weeklyPrices
              }
            )
        Right
        pricesOrError
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

getStockPriceData :: StockData -> IO (Maybe (Either StockPriceData String))
getStockPriceData (StockData pricesMVar) = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

