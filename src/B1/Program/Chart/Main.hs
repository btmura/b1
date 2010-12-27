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
  drawLoop (rotateAction False 0)
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

data Action = Action (IO Action)

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

rotateAction :: Bool -> GLfloat -> IO Action
rotateAction rotating rotateY = do
  loadIdentity
  doScale 0.5 0.5 0
  doRotate rotateY 0 1 0
  renderPrimitive LineLoop $ do
    drawVertex2 (-1) (-1)
    drawVertex2 (-1) 1
    drawVertex2 1 1
    drawVertex2 1 (-1)

  space <- getKey ' '

  if rotating && rotateY <= 180.00
    then return $ Action (rotateAction True (rotateY + 0.25))
    else case space of
      Press -> return $ Action (rotateAction True 0)
      Release -> return $ Action (rotateAction False 0)

doScale :: GLfloat -> GLfloat -> GLfloat -> IO ()
doScale x y z = scale x y z

doRotate :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
doRotate c x y z = rotate c $ Vector3 x y z

drawVertex2 :: GLfloat -> GLfloat -> IO () 
drawVertex2 x y = vertex $ Vertex2 x y
