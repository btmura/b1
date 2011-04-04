module B1.Program.Chart.VolumeBars
  ( VolumeBarsInput(..)
  , VolumeBarsOutput(..)
  , VolumeBarsState
  , drawVolumeBars
  , newVolumeBarsState
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data VolumeBarsInput = VolumeBarsInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: VolumeBarsState
  }

data VolumeBarsOutput = VolumeBarsOutput
  { outputState :: VolumeBarsState
  , isDirty :: Dirty
  }

data VolumeBarsState = VolumeBarsState

newVolumeBarsState :: VolumeBarsState
newVolumeBarsState = VolumeBarsState

drawVolumeBars :: Resources -> VolumeBarsInput -> IO VolumeBarsOutput
drawVolumeBars resources
    VolumeBarsInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state
      } = do
  preservingMatrix $ do
    color $ blue alpha
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1
    drawSquarePlaceholder
  return VolumeBarsOutput
    { outputState = state
    , isDirty = False
    }

