module B1.Program.Chart.Resources
  ( Resources (..)
  , updateWindowSize
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

data Resources = Resources
  { font :: Font
  , windowWidth :: Int
  , windowHeight :: Int
  } deriving (Show, Eq)

updateWindowSize :: Size -> Resources -> Resources
updateWindowSize (Size width height) resources = resources
  { windowWidth = fromIntegral width
  , windowHeight = fromIntegral height
  }


