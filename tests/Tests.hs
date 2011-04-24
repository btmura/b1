import Test.Framework
import Test.Framework.Providers.QuickCheck2

import qualified B1.Data.ListTest
import qualified B1.Data.Price.GoogleTest
import qualified B1.Data.Price.MockTest
import qualified B1.Data.PriceTest
import qualified B1.Data.RangeTest
import qualified B1.Data.String.UtilsTest
import qualified B1.Data.Technicals.StochasticTest
import qualified B1.Program.Chart.ConfigTest
import qualified B1.Program.Chart.ResourcesTest

main :: IO ()
main = defaultMain tests

tests =
  [ B1.Data.ListTest.getTestGroup
  , B1.Data.Price.GoogleTest.getTestGroup
  , B1.Data.Price.MockTest.getTestGroup
  , B1.Data.PriceTest.getTestGroup
  , B1.Data.RangeTest.getTestGroup
  , B1.Data.String.UtilsTest.getTestGroup
  , B1.Data.Technicals.StochasticTest.getTestGroup
  , B1.Program.Chart.ConfigTest.getTestGroup
  , B1.Program.Chart.ResourcesTest.getTestGroup
  ]

