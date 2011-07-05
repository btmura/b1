module B1.Program.Chart.ResourcesTest
  ( getTestGroup
  ) where

import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import B1.Control.TaskManager
import B1.Graphics.Rendering.OpenGL.BufferManager

import qualified Test.Framework.Providers.API

import B1.Program.Chart.Resources

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Program.Chart.ResourcesTest"
  [ testCase "case_updateMousePosition" case_updateMousePosition
  , testCase "case_invertMousePositionY" case_invertMousePositionY
  , testCase "case_updateWindowSize" case_updateWindowSize
  ]

case_updateMousePosition :: Assertion
case_updateMousePosition = do
  origResources <- createResources
  let position = (1337, 3007)
      glPosition = Position 1337 3007
      expectedResources = origResources
        { mousePosition = position
        }
  assertEqual "" expectedResources
      (updateMousePosition glPosition origResources)

case_invertMousePositionY :: Assertion
case_invertMousePositionY = do
  baseResources <- createResources
  let origResources = baseResources
        { windowWidth = 1337
        , windowHeight = 3007
        , mousePosition = (42, 1007)
        }
      expectedResources = origResources
        { mousePosition = (42, 2000) 
        }
  assertEqual "" expectedResources (invertMousePositionY origResources)

case_updateWindowSize :: Assertion
case_updateWindowSize = do
  origResources <- createResources
  let width = 1337
      height = 3007
      size = Size width height
      expectedResources = origResources
        { windowWidth = realToFrac width
        , windowHeight = realToFrac height
        }
  assertEqual "" expectedResources (updateWindowSize size origResources) 

createResources :: IO Resources
createResources = do
  font <- createTextureFont "noSuchFont"
  [program] <- genObjectNames 1
  bufferManager <- newBufferManager
  taskManager <- newTaskManager
  return $ newResources False font program bufferManager taskManager

