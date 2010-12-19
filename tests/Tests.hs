import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified B1.Data.Price.GoogleTest
import qualified B1.Data.Price.MockTest

main :: IO ()
main = defaultMain tests

tests =
  [ B1.Data.Price.GoogleTest.getTestGroup
  , B1.Data.Price.MockTest.getTestGroup
  ]

