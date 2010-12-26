module B1.Program.GetPrices.Main
  ( main
  ) where
  
import Data.Time
import System.Environment

import B1.Data.Price.Google
import B1.Program.GetPrices.Options

main = do
  args <- getArgs
  (options, nonOptions) <- readOptions args

  putStrLn $ "Symbol: " ++ symbol options
  putStrLn $ "Data source: " ++ show (dataSource options)

  now <- getCurrentDate
  prices <- getGooglePrices (getStartDate now) (getEndDate now)
      (symbol options)
  putStrLn $ "Prices: " ++ show prices

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

