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
    (2, 0, 0) -> do
      exceptionOrResult <- try $ return (parseGoogleCsv (rspBody response))
      either handleParseException handleParseResult exceptionOrResult
    _ -> do
      hPutStrLn stderr ("getGooglePrices response code: "
          ++ show responseCode)
      return Nothing

handleParseException :: SomeException -> IO (Maybe [Price])
handleParseException exception = do
  hPutStrLn stderr $ "getGooglePrices parse exception: " ++ show exception
  return Nothing

handleParseResult :: [Price] -> IO (Maybe [Price])
handleParseResult = return . Just . id

-- | Parses the CSV response from Google Finance.
-- Throws exceptions if there are any problems.
-- Exposed only for testing purposes.
parseGoogleCsv :: String -> [Price]
parseGoogleCsv = map (createPrice . split ',') . drop 1 . split '\n'

createPrice :: [String] -> Price
createPrice (date:open:high:low:close:volume:_) = Price
  { startTime = readDateString date
  , endTime = readDateString date
  , open = read open::Float
  , high = read high::Float
  , low = read low::Float
  , close = read close::Float
  , volume = read volume::Int
  }

readDateString :: String -> LocalTime
readDateString = readTime defaultTimeLocale "%e-%b-%g"

