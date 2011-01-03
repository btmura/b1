module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Data.IORef
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Screen

main :: IO ()
main = do
  initialize
  createWindow

  resourcesRef <- createInitialResources
  windowSizeCallback $= myWindowSizeCallback resourcesRef
  drawLoop resourcesRef drawScreen

  closeWindow
  terminate

createWindow :: IO ()
createWindow = do
  openWindow (Size 400 400) [DisplayAlphaBits 8] Window
  windowTitle $= "B1"

  blendFunc $= (SrcAlpha, One)
  blend $= Enabled

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

  -- Use orthographic projection, since it is easier to position text.
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) 0 (realToFrac height)

  matrixMode $= Modelview 0
  loadIdentity

drawLoop :: IORef a -> (a -> IO (Action a Dirty, Dirty)) -> IO ()
drawLoop inputRef action = do
  clear [ColorBuffer, DepthBuffer]
  input <- readIORef inputRef
  (Action nextAction, isDirty) <- action input
  swapBuffers
  sleep 0.001

  esc <- getKey ESC
  case esc of
    Press -> return ()
    Release -> do
      -- If the screen is not dirty, then wait for events rather than drawing
      -- the same frame again and pegging the CPU to a 100%.
      unless isDirty waitEvents
      drawLoop inputRef nextAction

