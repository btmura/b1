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
  , stochasticsOrError :: Either [Stochastic] String
  , weeklyPricesOrError :: Either [Price] String
  , weeklyStochasticsOrError :: Either [Stochastic] String
  }

newStockData :: Symbol -> IO StockData
newStockData symbol = do
  priceDataMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate
    pricesOrError <- getGooglePrices startDate endDate symbol
    let stochasticsOrError = either
            (\prices -> Left $ getStochastics 10 3 prices)
            Right
            pricesOrError

        weeklyPricesOrError = either
            (\prices -> Left $ getWeeklyPrices prices)
            Right
            pricesOrError

        weeklyStochasticsOrError = either
            (\prices -> Left $ getStochastics 10 3 prices)
            Right
            weeklyPricesOrError

        stockData = StockPriceData
          { startDate = startDate
          , endDate = endDate
          , pricesOrError = pricesOrError
          , stochasticsOrError = stochasticsOrError
          , weeklyPricesOrError = weeklyPricesOrError
          , weeklyStochasticsOrError = weeklyStochasticsOrError
          }
    putMVar priceDataMVar $ trimStockData stockData
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

trimStockData :: StockPriceData -> StockPriceData
trimStockData
    stockPriceData@StockPriceData
      { pricesOrError = pricesOrError
      , stochasticsOrError = stochasticsOrError
      } = 
  stockPriceData
    { pricesOrError = trimmedPricesOrError
    , stochasticsOrError = trimmedStochasticsOrError
    }
  where
    lengthOfEither either = map length $ lefts [either]
    listLengths = concat
        [ lengthOfEither pricesOrError
        , lengthOfEither stochasticsOrError
        ]
    minLength = minimum listLengths
    trim = trimListOrError minLength
    trimmedPricesOrError = trim pricesOrError
    trimmedStochasticsOrError = trim stochasticsOrError

trimListOrError :: Int -> Either [a] String -> Either [a] String
trimListOrError length listOrError = either
    (\list -> Left $ take length list)
    (\error -> Right error)
    listOrError

getStockPriceData :: StockData -> IO (Maybe StockPriceData)
getStockPriceData (StockData pricesMVar) = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

