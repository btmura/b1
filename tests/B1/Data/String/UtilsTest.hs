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
  [ testCase "split_multipleElements" case_split_multipleElements
  , testCase "split_oneElement" case_split_oneElement
  , testCase "split_empty" case_split_empty
  ]

case_split_multipleElements :: Assertion  
case_split_multipleElements = assertEqual "" ["a","b","c"] (split ',' "a,b,c")

case_split_oneElement :: Assertion
case_split_oneElement = assertEqual "" ["a"] (split ',' "a")

case_split_empty :: Assertion
case_split_empty = assertEqual "" [] (split ',' "")

