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
  loadTextures

  resourcesRef <- createInitialResources
  windowSizeCallback $= myWindowSizeCallback resourcesRef
  keyCallback $= myKeyCallback resourcesRef
  mousePosCallback $= myMousePosCallback resourcesRef
  mouseButtonCallback $= myMouseButtonCallback resourcesRef

  drawLoop resourcesRef drawScreen

  closeWindow
  terminate

createWindow :: IO ()
createWindow = do
  openWindow (Size 400 400) [DisplayAlphaBits 8] Window
  windowTitle $= "B1"

  blendFunc $= (SrcAlpha, One)
  blend $= Enabled

loadTextures :: IO ()
loadTextures = do
  mapM_ (uncurry bindTexture) (zip [0 ..] fileNames)
  texture Texture2D $= Disabled
  where
    fileNames =
      [ "res/add-normal.tga"
      , "res/add-hover.tga"
      , "res/add-press.tga"
      ]

bindTexture :: Int -> String -> IO ()
bindTexture textureNumber fileName = do
  textureBinding Texture2D $= Just (TextureObject (fromIntegral textureNumber))
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  loadResult <- loadTexture2D fileName [BuildMipMaps]
  putStrLn $ "Loading texture " ++ fileName ++ ": " ++ show loadResult

-- | Initialize the resources that should be immutable like fonts.
-- The other fields will be filled in later.
createInitialResources :: IO (IORef Resources)
createInitialResources = do
  font <- createTextureFont "res/fonts/orbitron/orbitron-medium.ttf"
  newIORef $ newResources font

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

myKeyCallback :: IORef Resources -> Key -> KeyButtonState -> IO ()
myKeyCallback resourcesRef key state = do
  let maybeKey = if state == Press then Just key else Nothing
  modifyIORef resourcesRef $ updateKeyPress maybeKey

myMousePosCallback :: IORef Resources -> Position -> IO ()
myMousePosCallback resourcesRef position =
  modifyIORef resourcesRef $ updateMousePosition position

myMouseButtonCallback :: IORef Resources -> MouseButton -> KeyButtonState
    -> IO ()
myMouseButtonCallback resourcesRef button state =
  modifyIORef resourcesRef $ updateMouseButton button state

drawLoop :: IORef Resources -> (Resources -> IO (Action Resources Dirty, Dirty))
    -> IO ()
drawLoop resourcesRef action = do
  clear [ColorBuffer, DepthBuffer]
  resources <- readIORef resourcesRef
  (Action nextAction, isDirty) <- action resources

  -- Reset the key pressed after each frame.
  modifyIORef resourcesRef $ updateKeyPress Nothing

  swapBuffers
  sleep 0.001

  control <- getKey LCTRL
  c <- getKey 'C'
  unless (control == Press && c == Press) $ do
    -- If the screen is not dirty, then wait for events rather than drawing
    -- the same frame again and pegging the CPU to a 100%.
    unless isDirty waitEvents
    drawLoop resourcesRef nextAction

