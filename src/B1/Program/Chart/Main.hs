module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action

main :: IO ()
main = do
  initialize
  createWindow
  drawLoop $ comboAction
      (rotateAction '1' (Color3 (1::GLfloat) 0 0) 0.25 False 0)
      (comboAction 
          (rotateAction '2' (Color3 (0::GLfloat) 0 1) 0.5 False 0)
          (rotateAction '3' (Color3 (0::GLfloat) 1 0) 0.75 False 0))
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

rotateAction :: Char -> Color3 GLfloat -> GLfloat -> Bool -> GLfloat -> IO Action
rotateAction key newColor scaleFactor rotating rotateY = do
  loadIdentity
  doScale scaleFactor scaleFactor 0
  doRotate rotateY 0 1 0
  color newColor
  renderPrimitive LineLoop $ do
    drawVertex2 (-1) (-1)
    drawVertex2 (-1) 1
    drawVertex2 1 1
    drawVertex2 1 (-1)

  space <- getKey key

  if rotating && rotateY <= 180.00
    then return $ Action (rotateAction key newColor scaleFactor True (rotateY + 0.25))
    else case space of
      Press -> return $ Action (rotateAction key newColor scaleFactor True 0)
      Release -> return $ Action (rotateAction key newColor scaleFactor False 0)

doScale :: GLfloat -> GLfloat -> GLfloat -> IO ()
doScale x y z = scale x y z

doRotate :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
doRotate c x y z = rotate c $ Vector3 x y z

drawVertex2 :: GLfloat -> GLfloat -> IO () 
drawVertex2 x y = vertex $ Vertex2 x y
