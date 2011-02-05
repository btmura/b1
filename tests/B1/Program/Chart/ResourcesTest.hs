module B1.Program.Chart.ResourcesTest
  ( getTestGroup
  ) where

import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Test.Framework
import qualified Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import B1.Program.Chart.Resources

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Program.Chart.ResourcesTest"
  [ testCase "case_updateMouseButton" case_updateMouseButton
  , testCase "case_updateMousePosition" case_updateMousePosition
  , testCase "case_updateWindowSize" case_updateWindowSize
  ]

case_updateMouseButton :: Assertion
case_updateMouseButton = do
  origResources <- newResources
  let expectedResources = origResources
        { leftMouseButtonPressed = True
        }
  assertEqual "" expectedResources
      (updateMouseButton ButtonLeft Press origResources)

case_updateMousePosition :: Assertion
case_updateMousePosition = do
  origResources <- newResources
  let position = (1337, 3007)
      glPosition = Position 1337 3007
      expectedResources = origResources
        { mousePosition = position
        }
  assertEqual "" expectedResources
      (updateMousePosition glPosition origResources)

case_updateWindowSize :: Assertion
case_updateWindowSize = do
  origResources <- newResources
  let width = 1337
      height = 3007
      size = Size width height
      expectedResources = origResources
        { windowWidth = realToFrac width
        , windowHeight = realToFrac height
        }
  assertEqual "" expectedResources (updateWindowSize size origResources) 

newResources :: IO Resources
newResources = do
  font <- createTextureFont "noSuchFont"
  return $  Resources
    { font = font
    , windowWidth = 0
    , windowHeight = 0
    , sideBarWidth = 0
    , keyPress = Nothing
    , mousePosition = (0, 0)
    , leftMouseButtonPressed = False
    }


