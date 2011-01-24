module B1.Program.Chart.Instructions
  ( InstructionsState (..)
  , drawInstructions
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources

data InstructionsState = InstructionsState
  { width :: GLfloat
  , height :: GLfloat
  , alpha :: GLfloat
  }

drawInstructions :: Resources -> InstructionsState
    -> IO (InstructionsState, Dirty)
drawInstructions resources@Resources { layout = layout }
    state@InstructionsState { width = width, alpha = alpha } = do
  [left, bottom, right, top] <- prepareTextLayout resources fontSize
      layoutLineLength instructions

  let textCenterX = -(left + abs (right - left) / 2)
      textCenterY = -(top - abs (bottom - top) / 2)

  color $ green alpha
  preservingMatrix $ do 
    translate $ vector3 textCenterX textCenterY 0
    renderLayout layout instructions

  return (state, False)

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    instructions = "Type in symbol and press ENTER..."

