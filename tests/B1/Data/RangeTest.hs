module B1.Data.RangeTest
  ( getTestGroup
  ) where

import Data.Time
import Test.Framework
import qualified Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import B1.Data.Range

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.RangeTest"
  [ testCase "case_gradualRange_increasing" case_gradualRange_increasing
  , testCase "case_gradualRange_decreasing" case_gradualRange_decreasing
  , testProperty "prop_gradualRange_increasing" prop_gradualRange_increasing
  , testProperty "prop_gradualRange_count" prop_gradualRange_count
  ]

case_gradualRange_increasing :: Assertion
case_gradualRange_increasing =
  assertEqual "" [0.0, 0.5, 0.75, 1.0] (gradualRange 0 1 2)

case_gradualRange_decreasing :: Assertion
case_gradualRange_decreasing =
  assertEqual "" [1.0, 0.5, 0.25, 0.0] (gradualRange 1 0 2)

prop_gradualRange_increasing :: Float -> Float -> Int -> Property
prop_gradualRange_increasing start end steps =
  steps >= 0 && steps < 1000 && start <= end ==>
    all (\(x, y) -> x <= y) $ pairList (gradualRange start end steps)

prop_gradualRange_decreasing :: Float -> Float -> Int -> Property
prop_gradualRange_decreasing start end steps =
  steps >= 0 && steps < 1000 && start <= end ==>
    all (\(x, y) -> x >= y) $ pairList (gradualRange start end steps)

pairList :: [a] -> [(a, a)]
pairList [] = []
pairList [x] = []
pairList (x:y:[]) = [(x, y)]
pairList (x:y:xs) = (x,y) : pairList (y:xs)

prop_gradualRange_count :: Float -> Float -> Int -> Property
prop_gradualRange_count start end steps =
  steps >= 0 && steps < 1000 ==>
    steps + 2 == length (gradualRange start end steps)
