module B1.Program.Chart.Instructions
  ( InstructionsInput(..)
  , InstructionsOutput(..)
  , drawInstructions
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data InstructionsInput = InstructionsInput
  { bounds :: Box
  , alpha :: GLfloat
  }

data InstructionsOutput = InstructionsOutput
  { isDirty :: Dirty
  }

drawInstructions :: Resources -> InstructionsInput -> IO InstructionsOutput
drawInstructions Resources { font = font }
    InstructionsInput { alpha = alpha } = do

  color $ green alpha

  textBounds <- measureText textSpec
  let (centerX, centerY) = boxCenter textBounds
  preservingMatrix $ do 
    translate $ vector3 (-centerX) (-centerY) 0
    renderText textSpec

  return InstructionsOutput { isDirty = False }

  where
    textSpec = TextSpec font 18 "Type in symbol and press ENTER..."

