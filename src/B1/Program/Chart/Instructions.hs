module B1.Program.Chart.Instructions
  ( InstructionsInput(..)
  , InstructionsOutput(..)
  , drawInstructions
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources

data InstructionsInput = InstructionsInput
  { width :: GLfloat
  , alpha :: GLfloat
  }

data InstructionsOutput = InstructionsOutput
  { isDirty :: Dirty
  }

drawInstructions :: Resources -> InstructionsInput -> IO InstructionsOutput
drawInstructions resources
    InstructionsInput
      { width = width
      , alpha = alpha
      } = do
  [left, bottom, right, top] <- prepareLayoutText resources fontSize
      layoutLineLength instructions

  let textCenterX = -(left + abs (right - left) / 2)
      textCenterY = -(top - abs (bottom - top) / 2)

  color $ green alpha
  preservingMatrix $ do 
    translate $ vector3 textCenterX textCenterY 0
    renderLayoutText resources instructions

  return $ InstructionsOutput { isDirty = False }

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    instructions = "Type in symbol and press ENTER..."

