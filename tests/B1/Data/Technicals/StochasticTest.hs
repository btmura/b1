module B1.Data.Technicals.StochasticTest
  ( getTestGroup
  ) where

import Data.Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import B1.Data.Price
import B1.Data.Technicals.Stochastic

import qualified Test.Framework.Providers.API

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.Technicals.StochasticTest"
  [ testCase "case_getStochastics" case_getStochastics
  , testCase "case_kFast" case_kFast
  , testCase "case_simpleMovingAverage" case_simpleMovingAverage
  ]

case_getStochastics :: Assertion
case_getStochastics =
  assertEqual "" expected $ getStochastics kPeriods dPeriods prices
  where
    kPeriods = 3
    dPeriods = 2
    prices =
      [ createHighLowClosePrice 10 5 6
      , createHighLowClosePrice 20 10 11 
      , createHighLowClosePrice 30 15 16
      , createHighLowClosePrice 40 20 21
      , createHighLowClosePrice 50 25 26
      , createHighLowClosePrice 60 30 31
      ]
    kFastValues = 
      [ kFast [prices !! 0, prices !! 1, prices !! 2]
      , kFast [prices !! 1, prices !! 2, prices !! 3]
      , kFast [prices !! 2, prices !! 3, prices !! 4]
      , kFast [prices !! 3, prices !! 4, prices !! 5]
      ]
    kSlowValues =
      [ (kFastValues !! 0 + kFastValues !! 1) / 2
      , (kFastValues !! 1 + kFastValues !! 2) / 2
      , (kFastValues !! 2 + kFastValues !! 3) / 2
      ]
    dSlowValues =
      [ (kSlowValues !! 0 + kSlowValues !! 1) / 2
      , (kSlowValues !! 1 + kSlowValues !! 2) / 2
      ]
    expected = 
      [ Stochastic (kSlowValues !! 0) (dSlowValues !! 0)
      , Stochastic (kSlowValues !! 1) (dSlowValues !! 1)
      ]

case_kFast :: Assertion
case_kFast =
  assertEqual "" expected $ kFast prices
  where
    expected = (60 - 40) / (70 - 40) 
    prices =
      [ createHighLowClosePrice 70 50 60
      , createHighLowClosePrice 40 60 50
      , createHighLowClosePrice 20 40 30
      ]

case_simpleMovingAverage :: Assertion
case_simpleMovingAverage =
  assertEqual "" expected $ simpleMovingAverage numPeriods list
  where
    list = [1, 2, 3, 4, 5, 6]
    numPeriods = 3
    expected = [2, 3, 4, 5]

createHighLowClosePrice :: Float -> Float -> Float -> Price
createHighLowClosePrice high low close =
  Price
    { startTime = fakeTime
    , endTime = fakeTime
    , open = 0
    , high = high
    , low = low
    , close = close
    , volume = 0
    }
  where
    fakeTime = LocalTime
      { localDay = fromGregorian 2011 4 23
      , localTimeOfDay = midnight
      }
