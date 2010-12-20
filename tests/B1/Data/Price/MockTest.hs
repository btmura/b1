module B1.Data.Price.MockTest (getTestGroup) where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import B1.Data.Price.Mock

getTestGroup :: Test
getTestGroup = testGroup "B1.Data.Price.MockTest"
  [ testProperty "getMockPrices_numPrices" prop_getMockPrices_numPrices
  ]

prop_getMockPrices_numPrices :: Int -> Property
prop_getMockPrices_numPrices numPrices =
  numPrices >= 0 && numPrices < 1000 ==>
    numPrices == length (getMockPrices numPrices)
