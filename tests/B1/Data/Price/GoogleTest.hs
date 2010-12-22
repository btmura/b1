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
  [ testProperty "parseGoogleCsv_empty" prop_parseGoogleCsv_empty
  , testProperty "parseGoogleCsv_count" prop_parseGoogleCsv_count
  , testProperty "parseGoogleCsv_some" prop_parseGoogleCsv_some
  ]

prop_parseGoogleCsv_empty :: Bool
prop_parseGoogleCsv_empty = parseGoogleCsv input == output
  where
    input = "Data,Open,High,Low,Close,Volume\n"
    output = []

csv = "Date,Open,High,Low,Close,Volume\n"
    ++ "17-Dec-10,124.08,124.46,123.82,124.30,141075278\n"
    ++ "9-Dec-10,123.97,124.02,123.15,123.76,123705049\n"

prop_parseGoogleCsv_count :: Bool
prop_parseGoogleCsv_count = length (parseGoogleCsv csv) == 2

prop_parseGoogleCsv_some :: Bool
prop_parseGoogleCsv_some = parseGoogleCsv csv == output
  where
    output = 
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

