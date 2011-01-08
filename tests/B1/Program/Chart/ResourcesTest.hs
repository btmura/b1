module B1.Program.Chart.ResourcesTest
  ( getTestGroup
  ) where

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

  let width = 1337
      height = 3007
      size = Size (fromIntegral width) (fromIntegral height)

      origResources = Resources
        { font = font
        , keysPressed = []
        , windowWidth = 0
        , windowHeight = 0
        , currentSymbol = ""
        , nextSymbol = ""
        }

      expectedResources = Resources
        { font = font
        , keysPressed = []
        , windowWidth = width
        , windowHeight = 3007
        , currentSymbol = ""
        , nextSymbol = ""
        }
 
  assertEqual "" expectedResources (updateWindowSize size origResources) 
  where
 
