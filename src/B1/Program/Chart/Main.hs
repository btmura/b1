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
  drawLoop $ Just (drawAction 0)
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

data Action = Action (Maybe (IO Action))

drawLoop :: Maybe (IO Action) -> IO ()
drawLoop maybeAction = do
  case maybeAction of
    Just action -> do
      Action nextAction <- action
      drawLoop nextAction
    _ -> return ()

drawAction :: GLfloat -> IO Action
drawAction rotateY = do
  clear [ColorBuffer, DepthBuffer]
  loadIdentity

  doScale 0.5 0.5 1.0
  doRotate rotateY 0 1.0 0
  renderPrimitive LineLoop $ do
    drawVertex2 (-1) (-1)
    drawVertex2 (-1) 1
    drawVertex2 1 1
    drawVertex2 1 (-1)

  swapBuffers
  sleep 0.001

  esc <- getKey ESC
  if esc == Press
    then return $ Action Nothing
    else return $ Action $ Just $ drawAction $ rotateY + 0.2
    
doScale :: GLfloat -> GLfloat -> GLfloat -> IO ()
doScale x y z = scale x y z

doRotate :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
doRotate c x y z = rotate c $ Vector3 x y z

drawVertex2 :: GLfloat -> GLfloat -> IO () 
drawVertex2 x y = vertex $ Vertex2 x y
