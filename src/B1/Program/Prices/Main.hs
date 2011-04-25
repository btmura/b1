module B1.Program.Prices.Main
  ( main
  ) where
  
import Data.Either
import Data.Time
import System.Environment

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Price.Mock
import B1.Program.Prices.Options

main = do
  args <- getArgs
  (options, nonOptions) <- readOptions args

  putStrLn $ "Symbol: " ++ symbol options
  putStrLn $ "Data source: " ++ show (dataSource options)

  pricesOrError <- getPrices (dataSource options) (symbol options)
  either printAllPrices
      (\error -> putStrLn $ "  Error: " ++ error)
      pricesOrError

printAllPrices :: [Price] -> IO ()
printAllPrices prices = do
  printPrices prices
  printPrices $ getWeeklyPrices prices

printPrices :: [Price] -> IO ()
printPrices prices = do
  putStrLn $ "Prices (" ++ show (length prices) ++ "):" 
  mapM_ (\price -> putStrLn $ "  " ++ show price) prices
  putStrLn ""

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
  currentDate { localDay = addGregorianYearsClip (-1) day }

getEndDate :: LocalTime -> LocalTime
getEndDate currentDate = currentDate

