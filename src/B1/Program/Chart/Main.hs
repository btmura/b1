module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

main :: IO ()
main = do
  initialize
  createWindow
  drawLoop
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

drawLoop :: IO ()
drawLoop = do
  drawScene
  swapBuffers

  esc <- getKey ESC
  unless (esc == Press) $ do
    sleep 0.001
    drawLoop

drawScene :: IO ()
drawScene = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
