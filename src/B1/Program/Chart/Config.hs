module B1.Program.Chart.Config
  ( Config (..)
  , readConfig
  , writeConfig
  ) where

import Data.Maybe
import System.IO

import B1.Data.String.Utils
import B1.Data.Symbol

data Config = Config
  { symbols :: [Symbol]
  , selectedSymbol :: Maybe Symbol
  } deriving (Eq, Show, Read)

readConfig :: FilePath -> IO Config
readConfig filePath = do
  contents <- readFile filePath
  return $ case reads contents::[(Config, String)] of
    ((config, _):_) -> config
    _ -> Config
        { symbols = []
        , selectedSymbol = Nothing
        }

writeConfig :: FilePath -> Config -> IO ()
writeConfig filePath config = writeFile filePath $ show config

