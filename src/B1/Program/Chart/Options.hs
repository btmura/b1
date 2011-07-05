module B1.Program.Chart.Options
  ( Options(..)
  , readOptions
  ) where

import System.Console.GetOpt

data Options = Options

defaultOptions = Options

readOptions :: [String] -> IO (Options, [String])
readOptions args =
  case getOpt RequireOrder options args of
    (options, nonOptions, []) -> 
      return (foldl (flip id) defaultOptions options, nonOptions)
    (_, _, errors) ->
      ioError (userError (concat errors ++ usageInfo header options))
  where
    progName = head args
    header = "Usage: " ++ progName ++ " [OPTION...]"

options :: [OptDescr (Options -> Options)]
options = []

