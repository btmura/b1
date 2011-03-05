module B1.Program.Chart.Colors
  ( black
  , blue
  , lightBlue
  , lighterBlue
  , gray
  , green
  , white
  , outlineColor
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Resources

black :: GLfloat -> Color4 GLfloat
black = color4 0 0 0

blue :: GLfloat -> Color4 GLfloat
blue = color4 0 0.25 0.5 

lightBlue :: GLfloat -> Color4 GLfloat
lightBlue = color4 0 0.25 0.75

lighterBlue :: GLfloat -> Color4 GLfloat
lighterBlue = color4 0 0.25 1

gray :: GLfloat -> Color4 GLfloat
gray = color4 0.5 0.5 0.5

green :: GLfloat -> Color4 GLfloat
green = color4 0.25 1 0

white :: GLfloat -> Color4 GLfloat
white = color4 1 1 1

outlineColor :: Resources -> Box -> GLfloat -> Color4 GLfloat
outlineColor resources@Resources { mousePosition = mousePosition } bounds
  | clicked = lighterBlue
  | hover = lightBlue
  | otherwise = blue
  where 
    -- TODO: Make helper method for hover and click states.
    hover = boxContains bounds mousePosition
    clicked = hover && isMouseButtonClicked resources ButtonLeft

