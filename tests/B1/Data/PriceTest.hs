module B1.Data.PriceTest
  ( getTestGroup
  ) where

import Data.Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import B1.Data.Price

import qualified Test.Framework.Providers.API

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.PriceTest"
  [ testCase "case_getWeeklyPrices" case_getWeeklyPrices
  ]

case_getWeeklyPrices :: Assertion
case_getWeeklyPrices =
  assertEqual "" expected $ getWeeklyPrices prices
  where
    prices =
      [ Price
        { startTime = createTime 2011 4 26
        , endTime = createTime 2011 4 26
        , open = 5 
        , high = 10
        , low = 2
        , close = 8
        , volume = 50
        }
      , Price
        { startTime = createTime 2011 4 22
        , endTime = createTime 2011 4 22
        , open = 10 
        , high = 20
        , low = 5
        , close = 15
        , volume = 100
        }
      , Price
        { startTime = createTime 2011 4 21
        , endTime = createTime 2011 4 21
        , open = 17
        , high = 25
        , low = 1
        , close = 18
        , volume = 200
        }
      ]
    expected =
      [ Price
        { startTime = createTime 2011 4 26
        , endTime = createTime 2011 4 26
        , open = 5 
        , high = 10
        , low = 2
        , close = 8
        , volume = 50
        }
      , Price
        { startTime = createTime 2011 4 21
        , endTime = createTime 2011 4 22
        , open = 17 
        , high = 25
        , low = 1
        , close = 15
        , volume = 300
        }
      ]

createTime :: Integer -> Int -> Int -> LocalTime
createTime year month day = LocalTime
  { localDay = fromGregorian year month day
  , localTimeOfDay = midnight
  }

