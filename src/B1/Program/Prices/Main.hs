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
  currentDate { localDay = addGregorianYearsClip (-3) day }

getEndDate :: LocalTime -> LocalTime
getEndDate currentDate = currentDate

printInfo :: [Price] -> IO ()
printInfo stockPrices = do
  printPrices "Prices " $ prices stockData
  printPrices "Trimmed Prices " $
      take (numDailyElements stockData) $ prices stockData

  printPrices "Weekly Prices " $ weeklyPrices stockData
  printPrices "Trimmed Weekly Prices " $
      take (numWeeklyElements stockData) $ weeklyPrices stockData

  printStochastics "Stochastics " $ stochastics stockData
  printStochastics "Weekly Stochastics " $ weeklyStochastics stockData

  where
    weeklyStockPrices = getWeeklyPrices stockPrices
    stockData = createStockPriceData stockPrices

printPrices :: String -> [Price] -> IO ()
printPrices label prices = do
  putStrLn $ label ++ " (" ++ show (length prices) ++ "):" 
  mapM_ (\price -> putStrLn $ "  " ++ show price) prices
  putStrLn ""

printStochastics :: String -> [Stochastic] -> IO ()
printStochastics label stochastics = do
  putStrLn $ label ++ " (" ++ show (length stochastics) ++ "):" 
  mapM_ (\stochastic -> putStrLn $ "  " ++ show stochastic) stochastics
  putStrLn ""

