module B1.Data.Price.GoogleTest
  ( getTestGroup
  ) where

import Data.Either
import Data.Time
import Test.Framework
import qualified Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import B1.Data.Price
import B1.Data.Price.Google

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.Price.GoogleTest"
  [ testCase "case_parseGoogleCsv_good" case_parseGoogleCsv_good
  , testCase "case_parseGoogleCsv_missingField" case_parseGoogleCsv_missingField
  , testCase "case_parseGoogleCsv_invalidField" case_parseGoogleCsv_invalidField
  , testCase "case_parseGoogleCsv_badFormat" case_parseGoogleCsv_badFormat
  , testCase "case_parseGoogleCsv_noLines" case_parseGoogleCsv_noLines
  , testCase "case_parseGoogleCsv_nothing" case_parseGoogleCsv_nothing
  ]

headers = "Date,Open,High,Low,Close,Volume\n"

goodLines =
  [ "17-Dec-10,124.08,124.46,123.82,124.30,141075278\n"
  , "9-Dec-10,123.97,124.02,123.15,123.76,123705049\n"
  ]

goodPrices =
  [ Price
    { startTime = LocalTime (fromGregorian 2010 12 17) midnight
    , endTime = LocalTime (fromGregorian 2010 12 17) midnight
    , open = 124.08
    , high = 124.46
    , low = 123.82
    , close = 124.30
    , volume = 141075278
    }
  , Price
    { startTime = LocalTime (fromGregorian 2010 12 9) midnight
    , endTime = LocalTime (fromGregorian 2010 12 9) midnight
    , open = 123.97
    , high = 124.02
    , low = 123.15
    , close = 123.76
    , volume = 123705049
    }
  ]

missingField = "17-Dec-10,124.46,123.82,124.30,141075278\n"

invalidField = "9-Dec-10,Error,124.02,123.15,123.76,123705049\n"

badFormat = "Error 404\n"

case_parseGoogleCsv_good :: Assertion
case_parseGoogleCsv_good =
  let csv = foldl (++) "" (headers:goodLines)
  in assertEqual "" (Left goodPrices) (parseGoogleCsv csv) 

case_parseGoogleCsv_missingField :: Assertion
case_parseGoogleCsv_missingField =
  let csv = foldl (++) "" ([headers] ++ goodLines ++ [missingField])
  in assertEqual "" (Right "Invalid CSV format") (parseGoogleCsv csv) 

case_parseGoogleCsv_invalidField :: Assertion
case_parseGoogleCsv_invalidField =
  let csv = foldl (++) "" ([headers] ++ goodLines ++ [invalidField])
  in assertEqual "" (Right "Invalid CSV format") (parseGoogleCsv csv) 

case_parseGoogleCsv_badFormat :: Assertion
case_parseGoogleCsv_badFormat =
  let csv = foldl (++) "" ([headers] ++ goodLines ++ [badFormat])
  in assertEqual "" (Right "Invalid CSV format") (parseGoogleCsv csv) 

case_parseGoogleCsv_noLines :: Assertion
case_parseGoogleCsv_noLines =
  assertEqual "" (Left []) (parseGoogleCsv headers)

case_parseGoogleCsv_nothing :: Assertion
case_parseGoogleCsv_nothing =
  assertEqual "" (Right "Invalid CSV format") (parseGoogleCsv "") 
