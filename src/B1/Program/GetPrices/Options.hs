module B1.Program.GetPrices.Options
  ( Options(..)
  , DataSource(..)
  , readOptions
  ) where

import System.Console.GetOpt

data Options = Options
  { dataSource :: DataSource
  , symbol :: String
  }

data DataSource = Google | Mock deriving Show

defaultOptions = Options
  { dataSource = Google
  , symbol = "SPY"
  }

readOptions :: [String] -> IO (Options, [String])
readOptions args =
  case getOpt RequireOrder options args of
    (options, nonOptions, []) -> 
      return (foldl (flip id) defaultOptions options, nonOptions)
    (_, _, errors) ->
      ioError (userError (concat errors ++ usageInfo header options))
  where
    header = "Usage: b1 [OPTION...]"

options :: [OptDescr (Options -> Options)]
options =
  [ Option "d" ["dataSource"] (ReqArg getDataSource "DATASOURCE")
      "Data source to get stock price information."
  , Option "s" ["symbol"] (ReqArg getSymbol "SYMBOL")
      "Stock symbol like SPY."
  ]

getDataSource :: String -> Options -> Options
getDataSource arg options = options { dataSource = readDataSource } 
  where
    readDataSource = case arg of
      "google" -> Google
      "mock" -> Mock
      _ -> Google

getSymbol :: String -> Options -> Options
getSymbol arg options = options { symbol = arg }


