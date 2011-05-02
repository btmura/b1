module B1.Data.Technicals.StockData
  ( StockData
  , StockPriceData(..)
  , newStockData
  , createStockPriceData
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
import B1.Data.Symbol
import B1.Data.Technicals.Stochastic

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
    putMVar priceDataMVar $
        either (Left . createStockPriceData) Right pricesOrError
  return $ StockData priceDataMVar

getStartDate :: IO LocalTime
getStartDate = do
  endDate <- getEndDate
  let yearAgo = addGregorianYearsClip (-3) (localDay endDate)
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

createStockPriceData :: [Price] -> StockPriceData
createStockPriceData prices = 
  StockPriceData
    { prices = finalPrices
    , stochastics = finalStochastics
    , weeklyPrices =  trimmedWeeklyPrices
    , weeklyStochastics = trimmedWeeklyStochastics
    }
  where
    stochasticsFunction = getStochastics 10 3
    stochastics = stochasticsFunction prices
    weeklyPrices = getWeeklyPrices prices
    weeklyStochastics = stochasticsFunction weeklyPrices

    dailyLength = min (length prices) (length stochastics)
    trimmedPrices = take dailyLength prices
    trimmedStochastics = take dailyLength stochastics 

    weeklyLength = min (length weeklyPrices) (length weeklyStochastics)
    trimmedWeeklyPrices = take weeklyLength weeklyPrices
    trimmedWeeklyStochastics = take weeklyLength weeklyStochastics

    weeklyStartTime = (startTime . last) trimmedWeeklyPrices
    finalPrices = takeWhile ((>= weeklyStartTime) . startTime) trimmedPrices
    finalStochastics = take (length finalPrices) stochastics

getStockPriceData :: StockData -> IO (Maybe (Either StockPriceData String))
getStockPriceData (StockData pricesMVar) = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

