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
  [ testCase "case_readConfig_writeConfig" case_readConfig_writeConfig
  ]

case_readConfig_writeConfig :: Assertion
case_readConfig_writeConfig = do
  assertReadWriteConfig $ Config { symbols = [] }
  assertReadWriteConfig $ Config { symbols = ["SPY"] }
  assertReadWriteConfig $ Config { symbols = ["SPY", "IWM"] }

assertReadWriteConfig :: Config -> Assertion
assertReadWriteConfig expectedConfig =
  bracket (openTestFile expectedConfig) (closeTestFile)
      (\(filePath, handle) -> do
        actualConfig <- readConfig handle 
        assertEqual "" expectedConfig actualConfig
        )

createTestFile :: Config -> IO String
createTestFile config = do
  bracket (openTempFile "/tmp" "config.test")
      (hClose . snd)
      (\(filePath, handle) -> do
        writeConfig handle config
        return filePath
      )

openTestFile :: Config -> IO (FilePath, Handle)
openTestFile config = do
  filePath <- createTestFile config
  handle <- openFile filePath ReadMode
  return (filePath, handle)

closeTestFile :: (FilePath, Handle) -> IO () 
closeTestFile (filePath, handle) = do
  hClose handle
  removeFile filePath


