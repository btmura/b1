module B1.Data.Technicals.StockData
  ( StockData
  , StockPriceData(..)
  , newStockData
  , refreshStockData
  , createStockPriceData
  , isLoading
  , isStockPriceData
  , isErrorMessage
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

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Symbol
import B1.Data.Technicals.MovingAverage
import B1.Data.Technicals.Stochastic

data StockData
  -- | Symbol is used when refreshing the stock data.
  = StockData Symbol (MVar (Either StockPriceData String))

-- TODO: Rename to something more different from StockData
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

newStockData :: Symbol -> IO StockData
newStockData symbol = do
  priceDataMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate
    pricesOrError <- getGooglePrices startDate endDate symbol
    putMVar priceDataMVar $
        either (Left . createStockPriceData) Right pricesOrError
  return $ StockData symbol priceDataMVar

refreshStockData :: StockData -> IO StockData
refreshStockData (StockData symbol _) = newStockData symbol

getStartDate :: IO LocalTime
getStartDate = do
  endDate <- getEndDate
  let startDay = (addDays (-13)
          . addDays (-200)
          . addGregorianYearsClip (-1)
          . localDay
          ) endDate
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
        , length movingAverage25
        , length movingAverage50
        , length movingAverage200
        ]

    maxWeeklyElements = minimum
        [ length weeklyPrices
        , length weeklyStochastics
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

isLoading :: StockData -> IO Bool
isLoading = handleStockData (ignore False) (ignore False) True

isStockPriceData :: StockData -> IO Bool
isStockPriceData = handleStockData (ignore True) (ignore False) False 

isErrorMessage :: StockData -> IO Bool
isErrorMessage = handleStockData (ignore False) (ignore True) False

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

{-- TODO: Replace getStockPriceData with this function...
getStockPriceData :: StockData -> IO (Maybe StockPriceData)
getStockPriceData = handleStockData Just (ignore Nothing) Nothing
--}

getStockPriceData :: StockData -> IO (Maybe (Either StockPriceData String))
getStockPriceData (StockData _ pricesMVar) = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

