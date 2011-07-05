module B1.Data.Technicals.StockData
  ( StockData
  , StockDataStatus(..)
  , StockPriceData(..)
  , newStockData
  , createStockPriceData
  , getStockDataStatus
  , getStockPriceData
  , getErrorMessage
  , handleStockData
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Either
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import System.IO

import B1.Control.TaskManager
import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Symbol
import B1.Data.Technicals.MovingAverage
import B1.Data.Technicals.Stochastic

-- TODO: Rename to StockDataFactory
data StockData = StockData Symbol (MVar (Either StockPriceData String))

-- TODO: Rename to StockData
data StockPriceData = StockPriceData
  { prices :: [Price]
  , stochastics :: [Stochastic]
  , weeklyPrices :: [Price]
  , weeklyStochastics :: [Stochastic]
  , movingAverage25 :: [MovingAverage]
  , movingAverage50 :: [MovingAverage]
  , movingAverage200 :: [MovingAverage]
  , numDailyElements :: Int
  , numWeeklyElements :: Int
  }

data StockDataStatus = Loading | Data | ErrorMessage deriving (Eq)

newStockData :: TaskManager -> Symbol -> IO StockData
newStockData taskManager symbol = do
  priceDataMVar <- newEmptyMVar
  addTask taskManager $ do
    startDate <- getStartDate
    endDate <- getEndDate
    pricesOrError <- getGooglePrices startDate endDate symbol
    putMVar priceDataMVar $
        either (Left . createStockPriceData) Right pricesOrError
  return $ StockData symbol priceDataMVar

getStartDate :: IO LocalTime
getStartDate = do
  endDate <- getEndDate
  let startDay = (addGregorianYearsClip (-2) . localDay) endDate
  return endDate
    { localDay = startDay
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
    { prices = prices
    , stochastics = stochastics
    , weeklyPrices = weeklyPrices
    , weeklyStochastics = weeklyStochastics
    , movingAverage25 = movingAverage25
    , movingAverage50 = movingAverage50
    , movingAverage200 = movingAverage200
    , numDailyElements = numDailyElements
    , numWeeklyElements = numWeeklyElements
    }
  where
    stochasticsFunction = getStochastics 10 3
    stochastics = stochasticsFunction prices

    movingAverage25 = getMovingAverage 25 prices
    movingAverage50 = getMovingAverage 50 prices
    movingAverage200 = getMovingAverage 200 prices

    weeklyPrices = getWeeklyPrices prices
    weeklyStochastics = stochasticsFunction weeklyPrices

    maxDailyElements = minimum
        [ length prices
        , length stochastics
        ]

    weeksInYear = 52

    maxWeeklyElements = minimum
        [ length weeklyPrices
        , length weeklyStochastics
        , weeksInYear
        ]

    earliestStartTime = (startTime
        . last
        . take maxWeeklyElements
        ) weeklyPrices

    numDailyElements = (length
        . takeWhile ((>= earliestStartTime) . startTime)
        . take maxDailyElements
        ) prices

    numWeeklyElements = (length
        . takeWhile ((>= earliestStartTime) . startTime) 
        . take maxWeeklyElements
        ) weeklyPrices

getStockDataStatus :: StockData -> IO StockDataStatus
getStockDataStatus = handleStockData (ignore Data) (ignore ErrorMessage) Loading

getStockPriceData :: StockData -> IO (Maybe StockPriceData)
getStockPriceData = handleStockData (return . Just) (ignore Nothing) Nothing

getErrorMessage :: StockData -> IO (Maybe String)
getErrorMessage = handleStockData (ignore Nothing) (return . Just) Nothing

handleStockData :: (StockPriceData -> IO a) -> (String -> IO a) -> a
    -> StockData -> IO a
handleStockData priceFunction errorFunction noValue
    (StockData _ contentsVar) = do
  maybeContents <- tryTakeMVar contentsVar
  case maybeContents of
    Just contents -> do
      tryPutMVar contentsVar contents
      either priceFunction errorFunction contents
    _ ->
      return noValue

ignore :: a -> b -> IO a
ignore value _ = return value


