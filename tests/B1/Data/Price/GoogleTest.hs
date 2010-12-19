module B1.Data.Price.GoogleTest (getTestGroup) where

import Data.List

import Test.Framework
import Test.Framework.Providers.QuickCheck2

getTestGroup = testGroup "B1.Data.Price.Google"
  [ testProperty "sort1" prop_sort1
  ]

prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])
