module B1.Program.Chart.Hud
  ( HudInput(..)
  , HudOutput(..)
  , HudState
  , drawHud
  , newHudState
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data HudInput = HudInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: HudState
  }

data HudOutput = HudOutput
  { outputState :: HudState
  , isDirty :: Dirty
  }

data HudState = HudState

newHudState :: HudState
newHudState = HudState

drawHud :: Resources -> HudInput -> IO HudOutput
drawHud resources
    input@HudInput
      { bounds = bounds
      , alpha = alpha
      , inputState = inputState
      } = do
  when (alpha > 0 && boxContains bounds (mousePosition resources)) $ do
    renderCrosshair resources bounds alpha
  return HudOutput
    { outputState = inputState
    , isDirty = False
    }

renderCrosshair :: Resources -> Box -> GLfloat -> IO ()
renderCrosshair resources bounds alpha =
  preservingMatrix $ do
    translateToWindowLowerLeft bounds

    let (mouseX, mouseY) = mousePosition resources
    renderPrimitive Lines $ do
      -- Vertical line
      color $ red4 0 
      vertex $ vertex2 mouseX (boxBottom bounds)

      color $ red4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ red4 0
      vertex $ vertex2 mouseX (boxTop bounds)

      -- Horizontal line
      color $ red4 0 
      vertex $ vertex2 (boxLeft bounds) mouseY

      color $ red4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ red4 0
      vertex $ vertex2 (boxRight bounds) mouseY

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0


