module B1.Data.Price.Google
  ( Prices
  , getGooglePrices
  , parseGoogleCsv
  ) where

import Control.Exception
import Data.Maybe
import Data.Time
import Network.HTTP hiding (close)
import Network.Stream hiding (close)
import System.IO
import System.Locale

import B1.Data.Price
import B1.Data.String.Utils

type Prices = (Maybe [Price], [String])

-- | Get price information using Google Finance.
-- Returns a tuple of Nothing and a list of error messages if tthere was a
-- network problem or parsing issue.
getGooglePrices :: LocalTime -> LocalTime -> String -> IO Prices
getGooglePrices startDate endDate symbol = priceErrorTuple
  where
    formatDate = formatTime defaultTimeLocale "%m/%d/%y"
    formattedStartDate = formatDate startDate
    formattedEndDate = formatDate endDate

    url = "http://www.google.com/finance/historical?output=csv&q=" ++ symbol
        ++ "&startDate=" ++ formattedStartDate
        ++ "&endDate=" ++ formattedEndDate

    priceErrorTuple = do
      exceptionOrResult <- try $ simpleHTTP (getRequest url)
      return $ either handleGetException handleGetResult exceptionOrResult

handleGetException :: SomeException -> Prices
handleGetException exception = (Nothing, [show exception])

handleGetResult :: Either ConnError (Response String) -> Prices
handleGetResult = either handleConError handleResponse 

handleConError :: ConnError -> Prices
handleConError connError = (Nothing, [show connError])

handleResponse :: Response String -> Prices
handleResponse response =
  case rspCode response of
    (2, 0, 0) -> parseGoogleCsv (rspBody response) 
    (x, y, z) -> (Nothing, ["Response error code: " ++ show x ++ show y ++ show z])

-- | Parses the CSV response from Google Finance.
-- Exposed only for testing purposes.
parseGoogleCsv :: String -> Prices
parseGoogleCsv = maybe (Nothing, ["Invalid CSV format"]) pricesOrNothing
    . maybe Nothing (Just . parsePriceLines)
    . dropHeader
    . split '\n'

dropHeader :: [String] -> Maybe [String]
dropHeader (_:rest) = Just rest
dropHeader _ = Nothing

parsePriceLines :: [String] -> [Maybe Price]
parsePriceLines = map (parsePriceTokens . split ',')

parsePriceTokens :: [String] -> Maybe Price
parsePriceTokens (date:open:high:low:close:volume:_) = maybePrice
  where
    maybeStartTime = parseDateString date
    maybeEndTime = parseDateString date
    maybeOpen = parseValue open::Maybe Float
    maybeHigh = parseValue high::Maybe Float
    maybeLow = parseValue low::Maybe Float
    maybeClose = parseValue close::Maybe Float
    maybeVolume = parseValue volume::Maybe Int

    maybePrice =
      if all isJust [maybeStartTime, maybeEndTime]
          && all isJust [maybeOpen, maybeHigh, maybeLow, maybeClose]
          && all isJust [maybeVolume]
        then Just Price
          { startTime = fromJust maybeStartTime
          , endTime = fromJust maybeEndTime
          , open = fromJust maybeOpen
          , high = fromJust maybeHigh
          , low = fromJust maybeLow
          , close = fromJust maybeClose
          , volume = fromJust maybeVolume
          }
        else Nothing

parsePriceTokens _ = Nothing

parseDateString :: String -> Maybe LocalTime
parseDateString = parseTime defaultTimeLocale "%e-%b-%g"

parseValue :: (Read a) => String -> Maybe a
parseValue string =
  case reads string of
    [(value, "")] -> Just value
    _ -> Nothing

pricesOrNothing :: [Maybe Price] -> (Maybe [Price], [String])
pricesOrNothing maybePrices =
  if all isJust maybePrices
    then (Just (catMaybes maybePrices), [])
    else (Nothing, ["Invalid CSV format"])

