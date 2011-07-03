module B1.Program.Prices.Main
  ( main
  ) where
  
import Data.Either
import Data.Time
import System.Environment

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Price.Mock
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Program.Prices.Options

main = do
  args <- getArgs
  (options, nonOptions) <- readOptions args

  putStrLn $ "Symbol: " ++ symbol options
  putStrLn $ "Data source: " ++ show (dataSource options)

  pricesOrError <- getPrices (dataSource options) (symbol options)
  either printInfo
      (\error -> putStrLn $ "  Error: " ++ error)
      pricesOrError

getPrices :: DataSource -> String -> IO (Either [Price] String)
getPrices Google symbol = do
  now <- getCurrentDate
  getGooglePrices (getStartDate now) (getEndDate now) symbol

getPrices Mock _ = return $ Left (getMockPrices 5)

getCurrentDate :: IO LocalTime
getCurrentDate = do
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ utcToLocalTime timeZone time

getStartDate :: LocalTime -> LocalTime
getStartDate currentDate@LocalTime { localDay = day } =
  currentDate { localDay = (addGregorianYearsClip (-1)) day }

getEndDate :: LocalTime -> LocalTime
getEndDate currentDate = currentDate

printInfo :: [Price] -> IO ()
printInfo stockPrices = do
  printData "Prices " $ prices stockData
  printData "Trimmed Prices " $
      take (numDailyElements stockData) $ prices stockData

  printData "Weekly Prices " $ weeklyPrices stockData
  printData "Trimmed Weekly Prices " $
      take (numWeeklyElements stockData) $ weeklyPrices stockData

  printData "Stochastics " $ stochastics stockData
  printData "Trimmed Stochastics " $
      take (numDailyElements stockData) $ stochastics stockData

  printData "Weekly Stochastics " $ weeklyStochastics stockData
  printData "Trimmed Weekly Stochastics " $
      take (numWeeklyElements stockData) $ weeklyStochastics stockData

  printData "Moving Average 200 " $ weeklyStochastics stockData
  printData "Moving Average 200" $
      take (numDailyElements stockData) $ movingAverage200 stockData

  where
    weeklyStockPrices = getWeeklyPrices stockPrices
    stockData = createStockPriceData stockPrices

printData :: Show a => String -> [a] -> IO ()
printData label values = do
  putStrLn $ label ++ " (" ++ show (length values) ++ "):" 
  mapM_ (\price -> putStrLn $ "  " ++ show price) values
  putStrLn ""

