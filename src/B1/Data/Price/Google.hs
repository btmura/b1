module B1.Data.Price.Google
  ( getGooglePrices
  , parseGoogleResponse
  ) where

import Control.Exception
import Network.HTTP
import Network.Stream
import System.IO

import B1.Data.Price

-- | Get price information using Google Finance.
-- Returns Nothing if there was a network problem getting the prices.
-- Prints out messages to standard error when there are network issues.
getGooglePrices :: String -> IO (Maybe [Price])
getGooglePrices symbol = do
  let url = "http://www.google.com/finance/historical?output=csv&q=" ++ symbol
  exceptionOrResult <- try $ simpleHTTP (getRequest url)
  either handleException handleResult exceptionOrResult

handleException :: SomeException -> IO (Maybe [Price])
handleException exception = do
  hPutStrLn stderr $ "getGooglePrices exception: " ++ (show exception)
  return Nothing

handleResult :: Either ConnError (Response String) -> IO (Maybe [Price])
handleResult = either handleConError handleResponse 

handleConError :: ConnError -> IO (Maybe [Price])
handleConError connError = do
  hPutStrLn stderr $ "getGooglePrices connection error: " ++ (show connError)
  return Nothing

handleResponse :: Response String -> IO (Maybe [Price])
handleResponse response = do
  let responseCode = rspCode response
  case responseCode of
    (2, 0, 0) -> return $ parseGoogleResponse (rspBody response) 
    _ -> do
      hPutStrLn stderr $ ("getGooglePrices response code: "
          ++ (show responseCode))
      return Nothing

-- | Parses the HTML response from Google Finance.
-- Exposed only for testing purposes.
parseGoogleResponse :: String -> Maybe [Price]
parseGoogleResponse _ = Just []
