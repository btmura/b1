module B1.Data.Price.Google
  ( getGooglePrices
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

-- | Get price information using Google Finance.
-- Returns Nothing if there was a network problem or parsing issues.
-- Prints out messages to standard error when there are network issues.
getGooglePrices :: String -> IO (Maybe [Price])
getGooglePrices symbol = do
  let url = "http://www.google.com/finance/historical?output=csv&q=" ++ symbol
  exceptionOrResult <- try $ simpleHTTP (getRequest url)
  either handleGetException handleGetResult exceptionOrResult

handleGetException :: SomeException -> IO (Maybe [Price])
handleGetException exception = do
  hPutStrLn stderr $ "getGooglePrices exception: " ++ show exception
  return Nothing

handleGetResult :: Either ConnError (Response String) -> IO (Maybe [Price])
handleGetResult = either handleConError handleResponse 

handleConError :: ConnError -> IO (Maybe [Price])
handleConError connError = do
  hPutStrLn stderr $ "getGooglePrices connection error: " ++ show connError
  return Nothing

handleResponse :: Response String -> IO (Maybe [Price])
handleResponse response = do
  let responseCode = rspCode response
  case responseCode of
    (2, 0, 0) -> return $ parseGoogleCsv (rspBody response) 
    _ -> do
      hPutStrLn stderr ("getGooglePrices response code: "
          ++ show responseCode)
      return Nothing

-- | Parses the CSV response from Google Finance.
-- Exposed only for testing purposes.
parseGoogleCsv :: String -> Maybe [Price]
parseGoogleCsv = maybe Nothing pricesOrNothing
    . maybe Nothing (Just . parsePriceLines)
    . dropHeader
    . split '\n'

dropHeader :: [String] -> Maybe [String]
dropHeader ("Date,Open,High,Low,Close,Volume":rest) = Just rest
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

pricesOrNothing :: [Maybe Price] -> Maybe [Price]
pricesOrNothing maybePrices =
  if all isJust maybePrices
    then Just $ catMaybes maybePrices
    else Nothing

