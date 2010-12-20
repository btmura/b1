module B1.Data.Price.GoogleTest (getTestGroup) where

import Data.List

import Test.Framework
import Test.Framework.Providers.QuickCheck2

getTestGroup = testGroup "B1.Data.Price.GoogleTest"
  [ testProperty "parseGoogleResponse" prop_parseGoogleResponse
  ]

prop_parseGoogleResponse = True
