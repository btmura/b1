module B1.Program.Chart.ResourcesTest
  ( getTestGroup
  ) where

import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Test.Framework
import qualified Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import B1.Program.Chart.Resources

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Program.Chart.ResourcesTest"
  [ testCase "case_updateWindowSize" case_updateWindowSize
  ]

case_updateWindowSize :: Assertion
case_updateWindowSize = do
  font <- createTextureFont "noSuchFont"
  layout <- createSimpleLayout

  let width = 1337
      height = 3007
      size = Size width height

      origResources = Resources
        { font = font
        , layout = layout
        , keyPress = Nothing
        , windowWidth = 0
        , windowHeight = 0
        , sideBarWidth = 0
        }

      expectedResources = Resources
        { font = font
        , layout = layout
        , keyPress = Nothing
        , windowWidth = realToFrac width
        , windowHeight = realToFrac height
        , sideBarWidth = 0
        }
 
  assertEqual "" expectedResources (updateWindowSize size origResources) 
  where
 
