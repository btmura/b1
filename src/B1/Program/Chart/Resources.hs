module B1.Program.Chart.Resources
  ( Resources (..)
  , updateKeysPressed
  , updateNextSymbol
  , updateWindowSize
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

data Resources = Resources
  { font :: Font
  , windowWidth :: Int
  , windowHeight :: Int
  , keysPressed :: [Key]
  , currentSymbol :: String
  , nextSymbol :: String
  } deriving (Show, Eq)

updateKeysPressed :: [Key] -> Resources -> Resources
updateKeysPressed keysPressed resources = resources
  { keysPressed = keysPressed
  }

updateNextSymbol :: Resources -> Resources
updateNextSymbol resources = resources

updateWindowSize :: Size -> Resources -> Resources
updateWindowSize (Size width height) resources = resources
  { windowWidth = fromIntegral width
  , windowHeight = fromIntegral height
  }


