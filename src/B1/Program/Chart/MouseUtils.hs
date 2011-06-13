module B1.Program.Chart.MouseUtils
  ( isMouseHoveringWithinBox
  , isMouseClickedWithinBox
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Graphics.Rendering.OpenGL.Box
import B1.Program.Chart.Resources

isMouseHoveringWithinBox :: Resources -> Box -> GLfloat -> Bool
isMouseHoveringWithinBox resources bounds alpha =
  alpha >= 1 && boxContains bounds (mousePosition resources)

isMouseClickedWithinBox :: Resources -> Box -> GLfloat -> Bool
isMouseClickedWithinBox resources bounds alpha =
  isMouseHoveringWithinBox resources bounds alpha
      && isMouseButtonClicked resources ButtonLeft

