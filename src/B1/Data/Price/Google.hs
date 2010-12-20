module B1.Data.Price.Google
  ( getGooglePrices
  ) where

import Control.Exception
import Network.HTTP
import Network.Stream
import System.IO

import B1.Data.Price

-- | Get price information using Google Finance. Returns Nothing if there was
-- | a network problem getting the prices or if there is no data for the
-- | given stock symbol. Prints out messages to standard error when there are
-- | network issues.
getGooglePrices :: String -> IO (Maybe String)
getGooglePrices symbol = do
  let url = "http://www.google.com/finance/historical?output=csv&q=" ++ symbol
  exceptionOrResult <- try $ simpleHTTP (getRequest url)
  either handleException handleResult exceptionOrResult

handleException :: SomeException -> IO (Maybe String)
handleException exception = do
  hPutStrLn stderr $ "getGooglePrices exception: " ++ (show exception)
  return Nothing

handleResult :: Either ConnError (Response String) -> IO (Maybe String)
handleResult = either handleConError handleResponse 

handleConError :: ConnError -> IO (Maybe String)
handleConError connError = do
  hPutStrLn stderr $ "getGooglePrices connection error: " ++ (show connError)
  return Nothing

handleResponse :: Response String -> IO (Maybe String)
handleResponse response = do
  let responseCode = rspCode response
  if responseCode == (2, 0, 0)
    then return $ Just (rspBody response)
    else do
      hPutStrLn stderr $ ("getGooglePrices response code: "
          ++ (show responseCode))
      return Nothing

