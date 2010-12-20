module B1.Data.String.UtilsTest
  ( getTestGroup
  ) where

import Test.Framework
import qualified Test.Framework.Providers.API 
import Test.Framework.Providers.HUnit
import Test.HUnit

import B1.Data.String.Utils

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Data.String.UtilsTest"
  [ testCase "split_1" case_split_1
  , testCase "split_2" case_split_2
  ]

case_split_1 :: Assertion  
case_split_1 = assertEqual "" ["a","b","c"] (split "a,b,c" ',')

case_split_2 :: Assertion
case_split_2 = assertEqual "" ["a"] (split "a" ',')

