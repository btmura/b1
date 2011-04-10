module B1.Program.Chart.ConfigTest
  ( getTestGroup
  ) where

import Control.Exception
import System.Directory
import System.IO
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Test.Framework.Providers.API

import B1.Program.Chart.Config

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Program.Chart.ConfigTest"
  [ testCase "case_readConfig_emptyFile" case_readConfig_emptyFile
  , testCase "case_readConfig_noSymbols" case_readConfig_noSymbols
  , testCase "case_readConfig_someSymbols" case_readConfig_someSymbols
  ]

case_readConfig_emptyFile :: Assertion
case_readConfig_emptyFile =
  assertConfig ""
      Config
        { symbols = []
        , selectedSymbol = Nothing
        }

case_readConfig_noSymbols :: Assertion
case_readConfig_noSymbols =
  assertConfig "Config { symbols = [], selectedSymbol = Nothing}"
      Config
        { symbols = []
        , selectedSymbol = Nothing
        }

case_readConfig_someSymbols :: Assertion
case_readConfig_someSymbols =
  assertConfig "Config { symbols = [\"SPY\"], selectedSymbol = Just \"IWM\"}" 
      Config
        { symbols = ["SPY"]
        , selectedSymbol = Just "IWM"
        }

assertConfig :: String -> Config -> Assertion
assertConfig contents expectedConfig =
  bracket (openTestFile contents)
      removeFile
      (\filePath -> do
        actualConfig <- readConfig filePath
        assertEqual "" expectedConfig actualConfig
      )

openTestFile :: String -> IO FilePath
openTestFile = createTestFile

createTestFile :: String -> IO String
createTestFile contents =
  bracket (openTempFile "/tmp" "config.test")
      (hClose . snd)
      (\(filePath, handle) -> do
        hPutStr handle contents
        return filePath
      )

