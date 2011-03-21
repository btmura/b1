module B1.Program.Chart.Config
  ( Config (..)
  , readConfig
  , writeConfig
  ) where

import System.IO

import B1.Data.String.Utils
import B1.Program.Chart.Symbol

data Config = Config
  { symbols :: [Symbol]
  } deriving (Eq, Show, Read)

readConfig :: Handle -> IO Config
readConfig handle = do
  contents <- hGetContents handle
  return $ case (reads contents)::[(Config, String)] of
    ((config, _):_) -> config
    _ -> Config { symbols = [] }

writeConfig :: Config -> Handle -> IO ()
writeConfig config handle = hPutStr handle $ show config

