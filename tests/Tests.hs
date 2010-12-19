import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.List

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "Test Group 1"
    [ testProperty "sort1" prop_sort1
    ]
  ]

prop_sort1 xs = sort xs == sortBy compare xs
  where types = (xs :: [Int])

