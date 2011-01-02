module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Data.IORef
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils

main :: IO ()
main = do
  initialize
  createWindow

  resourcesRef <- createInitialResources
  windowSizeCallback $= myWindowSizeCallback resourcesRef
  drawLoop resourcesRef $ drawScreen drawSideBar drawMainChart

  closeWindow
  terminate

createWindow :: IO ()
createWindow = do
  openWindow (Size 400 400) [DisplayAlphaBits 8] Window
  windowTitle $= "B1"

data Resources = Resources
  { font :: Font
  , windowWidth :: Int
  , windowHeight :: Int
  }

-- | Initialize the resources that should be immutable like fonts.
-- The other fields will be filled in later.
createInitialResources :: IO (IORef Resources)
createInitialResources = do
  font <- createTextureFont "res/DejaVuSans.ttf"
  newIORef Resources
    { font = font
    , windowWidth = 0
    , windowHeight = 0
    }

myWindowSizeCallback :: IORef Resources -> Size -> IO ()
myWindowSizeCallback resourcesRef size@(Size width height) = do
  viewport $= (Position 0 0, size)
  modifyIORef resourcesRef $ updateWindowSize size

  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) 0 (realToFrac height)

  matrixMode $= Modelview 0
  loadIdentity

updateWindowSize :: Size -> Resources -> Resources
updateWindowSize (Size width height) resources = resources
  { windowWidth = fromIntegral width
  , windowHeight = fromIntegral height
  }

drawLoop :: IORef a -> (a -> IO (Action a)) -> IO ()
drawLoop inputRef action = do
  clear [ColorBuffer, DepthBuffer]
  input <- readIORef inputRef
  Action nextAction <- action input
  swapBuffers
  sleep 0.001

  esc <- getKey ESC
  case esc of
    Press -> return ()
    Release -> drawLoop inputRef nextAction

-- TODO: Move this code into a separate Screen module.
drawScreen :: (a -> IO (Action a)) -> (a -> IO (Action a)) -> a -> IO (Action a)
drawScreen sideBarAction mainChartAction input = do
  Action nextSideBarAction <- sideBarAction input
  Action nextMainChartAction <- mainChartAction input
  return $ Action $ drawScreen nextSideBarAction nextMainChartAction

sideBarWidth = 175

drawSideBar :: Resources -> IO (Action Resources)
drawSideBar resources = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth / 2) (sideBarHeight / 2) 1
  color $ color3 0 0 1
  drawSquarePlaceholder
  return $ Action drawSideBar 

drawMainChart :: Resources -> IO (Action Resources)
drawMainChart resources = do
  let mainChartWidth = realToFrac (windowWidth resources) - sideBarWidth
      mainChartHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth + mainChartWidth / 2)
      (mainChartHeight / 2) 0

  preservingMatrix $ do
    color $ color3 0 1 0
    translate $ vector3 (-mainChartWidth / 2) (mainChartHeight / 2 - 24) 0
    setFontFaceSize (font resources) 24 72
    renderFont (font resources) "SPY" All

  scale3 (mainChartWidth / 2) (mainChartHeight / 2) 1
  color $ color3 1 0 0
  drawSquarePlaceholder
  return $ Action drawMainChart

