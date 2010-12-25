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
  , testProperty "parseGoogleCsv_some" prop_parseGoogleCsv_some
  , testProperty "parseGoogleCsv_valid" prop_parseGoogleCsv_valid
  , testProperty "parseGoogleCsv_noHeader" prop_parseGoogleCsv_noHeader
  , testProperty "parseGoogleCsv_badPrices" prop_parseGoogleCsv_badPrices
  ]

headers = "Date,Open,High,Low,Close,Volume\n"

csv = headers
    ++ "17-Dec-10,124.08,124.46,123.82,124.30,141075278\n"
    ++ "9-Dec-10,123.97,124.02,123.15,123.76,123705049\n"

badPrices = headers
    ++ "17-Dec-10,124.46,123.82,124.30,141075278\n"
    ++ "9-Dec-10,Error,123.15,123.76,123705049\n"

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

prop_parseGoogleCsv_valid :: Bool
prop_parseGoogleCsv_valid = parseGoogleCsv csv == Just prices

prop_parseGoogleCsv_noHeader :: String -> Bool
prop_parseGoogleCsv_noHeader noHeader =
  parseGoogleCsv noHeader == Nothing

prop_parseGoogleCsv_badPrices :: Bool
prop_parseGoogleCsv_badPrices =
  parseGoogleCsv badPrices == Nothing

prop_parseGoogleCsv_empty :: Bool
prop_parseGoogleCsv_empty = parseGoogleCsv headers == Just []

prop_parseGoogleCsv_some :: Bool
prop_parseGoogleCsv_some = parseGoogleCsv csv == Just prices

