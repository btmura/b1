module B1.Data.ListTest
  ( getTestGroup
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import B1.Data.List

import qualified Test.Framework.Providers.API

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.ListTest"
  [ testCase "case_groupElements" case_groupElements
  ]

case_groupElements :: Assertion
case_groupElements =
  assertEqual "" expected $ groupElements numPerGroup list
  where
    list = [1, 2, 3, 4, 5, 6]
    numPerGroup = 3
    expected = [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6]]

