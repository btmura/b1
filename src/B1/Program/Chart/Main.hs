module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Utils

data Resources = Resources
  { font :: Font
  }

main :: IO ()
main = do
  initialize
  createWindow
  resources <- createResources
  drawLoop $ drawScreen resources
  closeWindow
  terminate

createWindow :: IO ()
createWindow = do
  openWindow (Size 400 400) [DisplayAlphaBits 8] Window
  windowTitle $= "B1"
  windowSizeCallback $= myWindowSizeCallback

myWindowSizeCallback :: Size -> IO ()
myWindowSizeCallback size@(Size width height) = do
  viewport $= (Position 0 0, size)

  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) 0 (realToFrac height)

  matrixMode $= Modelview 0
  loadIdentity

createResources :: IO Resources
createResources = do
  font <- createTextureFont "res/DejaVuSans.ttf"
  return $ Resources { font = font }

drawLoop :: IO Action -> IO ()
drawLoop action = do
  clear [ColorBuffer, DepthBuffer]
  Action nextAction <- action
  swapBuffers
  sleep 0.001

  esc <- getKey ESC
  case esc of
    Press -> return ()
    Release -> drawLoop nextAction

drawScreen :: Resources -> IO Action
drawScreen resources = do
  loadIdentity

  (_, (Size width height)) <- get viewport

  let sideBarWidth = 150
      sideBarHeight = realToFrac height

  preservingMatrix $ do
    translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
    drawSideBar resources sideBarWidth sideBarHeight

  let mainChartWidth = realToFrac width - sideBarWidth
      mainChartHeight = realToFrac height

  preservingMatrix $ do
    translate $ vector3 (sideBarWidth + mainChartWidth / 2)
        (mainChartHeight / 2) 0
    drawMainChart resources mainChartWidth mainChartHeight

  return $ Action $ drawScreen resources

drawSideBar :: Resources -> GLfloat -> GLfloat -> IO ()
drawSideBar resources width height = do
  scale3 (realToFrac width / 2) (realToFrac height / 2) 1
  color $ color3 0 0 1
  drawSquare

drawMainChart :: Resources -> GLfloat -> GLfloat -> IO ()
drawMainChart resources width height = do
  preservingMatrix $ do
    color $ color3 0 1 0
    translate $ vector3 (-width / 2) (height / 2 - 24) 0
    setFontFaceSize (font resources) 24 72
    renderFont (font resources) "SPY" All

  scale3 (realToFrac width / 2) (realToFrac height / 2) 1
  color $ color3 1 0 0
  drawSquare

drawSquare :: IO ()
drawSquare = do
  renderPrimitive LineLoop $ do
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 1
    vertex $ vertex2 1 (-1)

  renderPrimitive Lines $ do
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 (-1)
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 1 1

