module B1.Program.Chart.Instructions
  ( InstructionsInput(..)
  , InstructionsOutput(..)
  , drawInstructions
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
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

  color $ green alpha

  boundingBox <- measureText textSpec
  let (centerX, centerY) = boxCenter boundingBox
  preservingMatrix $ do 
    translate $ vector3 (-centerX) (-centerY) 0
    renderText textSpec

  return $ InstructionsOutput { isDirty = False }

  where
    textSpec = TextSpec (font resources) 18 "Type in symbol and press ENTER..."

