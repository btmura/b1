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
  windowDirtyRef <- newIORef False
  windowSizeCallback $= myWindowSizeCallback resourcesRef windowDirtyRef
  mousePosCallback $= myMousePosCallback resourcesRef

  drawLoop resourcesRef windowDirtyRef drawScreen

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
      , "res/remove-normal.tga"
      , "res/remove-hover.tga"
      , "res/remove-press.tga"
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

myWindowSizeCallback :: IORef Resources -> IORef Dirty -> Size -> IO ()
myWindowSizeCallback resourcesRef windowDirtyRef size@(Size width height) = do
  viewport $= (Position 0 0, size)
  modifyIORef resourcesRef $ updateWindowSize size
  writeIORef windowDirtyRef True

  -- Use orthographic projection, since it is easier to position text.
  -- (0, 0) is lower left corner of the screen.
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (realToFrac width) 0 (realToFrac height)

  matrixMode $= Modelview 0
  loadIdentity

myMousePosCallback :: IORef Resources -> Position -> IO ()
myMousePosCallback resourcesRef position = do
  modifyIORef resourcesRef $
      (invertMousePositionY . updateMousePosition position)

drawLoop :: IORef Resources -> IORef Dirty
    -> (Resources -> IO (Action Resources Dirty, Dirty)) -> IO ()
drawLoop resourcesRef windowDirtyRef action = do
  clear [ColorBuffer, DepthBuffer]

  -- TODO: Use >>= syntax?
  refreshMouseButtonsPressed resourcesRef
  refreshKeysPressed resourcesRef
  resources <- readIORef resourcesRef

  (Action nextAction, isContentDirty) <- action resources

  swapBuffers
  sleep 0.001

  -- Screen may have been resized after swapBuffers even though
  -- there are no dirty animations.
  isWindowDirty <- readIORef windowDirtyRef
  writeIORef windowDirtyRef False

  control <- getKey LCTRL
  c <- getKey 'C'
  unless (control == Press && c == Press) $ do
    -- If the screen is not dirty, then wait for events rather than drawing
    -- the same frame again and pegging the CPU to a 100%.
    unless (isWindowDirty || isContentDirty) waitEvents
    drawLoop resourcesRef windowDirtyRef nextAction

refreshMouseButtonsPressed :: IORef Resources -> IO ()
refreshMouseButtonsPressed resourcesRef =
  updatePressed resourcesRef getMouseButton buttons updateMouseButtonsPressed
  where
    buttons = [ButtonLeft, ButtonRight]

refreshKeysPressed :: IORef Resources -> IO ()
refreshKeysPressed resourcesRef =
  updatePressed resourcesRef getKey keys updateKeysPressed
  where
    alphaKeys = map CharKey ['A'..'Z']
    specialKeys = map SpecialKey [ENTER, BACKSPACE, ESC]
    keys = alphaKeys ++ specialKeys

updatePressed :: IORef Resources -> (a -> IO KeyButtonState) -> [a]
    -> ([a] -> Resources -> Resources) -> IO ()
updatePressed resourcesRef ioFunction values updater = do
  states <- mapM ioFunction values
  let indexedStates = zip values states
      matchingStates = filter ((== Press) . snd) indexedStates
      matchingValues = map fst matchingStates
  modifyIORef resourcesRef $ updater matchingValues

