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
  ]

case_readConfig_emptyFile :: Assertion
case_readConfig_emptyFile =
  assertConfig "" $ Config { symbols = [] }

case_readConfig_noSymbols :: Assertion
case_readConfig_noSymbols =
  assertConfig "Config { symbols = [] }" $ Config { symbols = [] }

assertConfig :: String -> Config -> Assertion
assertConfig contents expectedConfig =
  bracket (openTestFile contents)
      closeTestFile
      (\(filePath, handle) -> do
        actualConfig <- readConfig handle
        assertEqual "" expectedConfig actualConfig
      )

openTestFile :: String -> IO (FilePath, Handle)
openTestFile contents = do
  filePath <- createTestFile contents
  handle <- openFile filePath ReadMode
  return (filePath, handle)

createTestFile :: String -> IO String
createTestFile contents = do
  bracket (openTempFile "/tmp" "config.test")
      (hClose . snd)
      (\(filePath, handle) -> do
        hPutStr handle contents
        return filePath
      )

closeTestFile :: (FilePath, Handle) -> IO () 
closeTestFile (filePath, handle) = do
  hClose handle
  removeFile filePath


