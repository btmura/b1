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
  return $ (read contents :: Config)

writeConfig :: Handle -> Config -> IO ()
writeConfig handle config = hPutStr handle $ show config

