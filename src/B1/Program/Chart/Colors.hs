module B1.Program.Chart.Colors
  ( black
  , blue
  , cyan
  , green
  , white
  , outlineColor
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Resources

black :: GLfloat -> Color4 GLfloat
black = color4 0 0 0

blue :: GLfloat -> Color4 GLfloat
blue = color4 0 0.25 0.5 

cyan :: GLfloat -> Color4 GLfloat
cyan = color4 0 0.25 1

green :: GLfloat -> Color4 GLfloat
green = color4 0.25 1 0

white :: GLfloat -> Color4 GLfloat
white = color4 1 1 1

outlineColor :: Resources -> Box -> GLfloat -> Color4 GLfloat
outlineColor (Resources { mousePosition = mousePosition }) bounds =
  if boxContains bounds mousePosition
    then cyan
    else blue
