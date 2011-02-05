module B1.Program.Chart.Resources
  ( Resources (..)
  , updateKeyPress
  , updateMousePosition
  , updateMouseButton
  , updateWindowSize
  ) where

import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

data Resources = Resources
  { font :: Font
  , windowWidth :: GLfloat
  , windowHeight :: GLfloat
  , sideBarWidth :: GLfloat
  , keyPress :: Maybe Key
  , mousePosition :: (GLfloat, GLfloat)
  , leftMouseButtonPressed :: Bool
  } deriving (Show, Eq)

updateKeyPress :: Maybe Key -> Resources -> Resources
updateKeyPress maybeKeyPress resources = resources
  { keyPress = maybeKeyPress
  }

updateMousePosition :: Position -> Resources -> Resources
updateMousePosition (Position x y) resources = resources
  { mousePosition = (fromIntegral x, fromIntegral y)
  }

updateMouseButton :: MouseButton -> KeyButtonState -> Resources -> Resources
updateMouseButton ButtonLeft Press resources = resources
  { leftMouseButtonPressed = True
  }

updateMouseButton _ _ resources = resources
  { leftMouseButtonPressed = False
  }

updateWindowSize :: Size -> Resources -> Resources
updateWindowSize (Size width height) resources = resources
  { windowWidth = fromIntegral width
  , windowHeight = fromIntegral height
  }


