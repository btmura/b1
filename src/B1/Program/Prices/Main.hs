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
{--
  printPrices stockPrices
  printStochastics $ getStochastics 10 3 stockPrices
  printPrices weeklyStockPrices
  printStochastics $ getStochastics 10 3 weeklyStockPrices
  putStrLn ""
  putStrLn ""
--}

  printPrices $ prices stockData
  printStochastics $ stochastics stockData
  printPrices $ weeklyPrices stockData
  printStochastics $ weeklyStochastics stockData
  where
    weeklyStockPrices = getWeeklyPrices stockPrices
    stockData = createStockPriceData stockPrices

printPrices :: [Price] -> IO ()
printPrices prices = do
  putStrLn $ "Prices (" ++ show (length prices) ++ "):" 
  mapM_ (\price -> putStrLn $ "  " ++ show price) prices
  putStrLn ""

printStochastics :: [Stochastic] -> IO ()
printStochastics stochastics = do
  putStrLn $ "Stochastics (" ++ show (length stochastics) ++ "):" 
  mapM_ (\stochastic -> putStrLn $ "  " ++ show stochastic) stochastics
  putStrLn ""

