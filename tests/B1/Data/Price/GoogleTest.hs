module B1.Data.Price.GoogleTest
  ( getTestGroup
  ) where

import Data.Time
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import B1.Data.Price
import B1.Data.Price.Google

getTestGroup :: Test
getTestGroup = testGroup "B1.Data.Price.GoogleTest"
  [ testProperty "readGoogleCsv_empty" prop_readGoogleCsv_empty
  , testProperty "readGoogleCsv_count" prop_readGoogleCsv_count
  , testProperty "readGoogleCsv_some" prop_readGoogleCsv_some
  , testProperty "parseGoogleCsv_valid" prop_parseGoogleCsv_valid
  , testProperty "parseGoogleCsv_invalid" prop_parseGoogleCsv_invalid
  ]

headers = "Date,Open,High,Low,Close,Volume\n"

csv = headers
    ++ "17-Dec-10,124.08,124.46,123.82,124.30,141075278\n"
    ++ "9-Dec-10,123.97,124.02,123.15,123.76,123705049\n"

prices =
  [ Price
    { startTime = LocalTime (fromGregorian 1910 12 17) midnight
    , endTime = LocalTime (fromGregorian 1910 12 17) midnight
    , open = 124.08
    , high = 124.46
    , low = 123.82
    , close = 124.30
    , volume = 141075278
    }
  , Price
    { startTime = LocalTime (fromGregorian 1910 12 9) midnight
    , endTime = LocalTime (fromGregorian 1910 12 9) midnight
    , open = 123.97
    , high = 124.02
    , low = 123.15
    , close = 123.76
    , volume = 123705049
    }
  ]

prop_parseGoogleCsv_valid :: IO Bool
prop_parseGoogleCsv_valid = do
  maybePrices <- parseGoogleCsv csv
  return $ maybePrices == Just prices

prop_parseGoogleCsv_invalid :: String -> IO Bool
prop_parseGoogleCsv_invalid invalid = do
  maybePrices <- parseGoogleCsv invalid
  return $ maybePrices == Nothing

prop_readGoogleCsv_empty :: Bool
prop_readGoogleCsv_empty = readGoogleCsv headers == []

prop_readGoogleCsv_count :: Bool
prop_readGoogleCsv_count = length (readGoogleCsv csv) == 2

prop_readGoogleCsv_some :: Bool
prop_readGoogleCsv_some = readGoogleCsv csv == prices

