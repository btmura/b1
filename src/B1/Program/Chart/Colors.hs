module B1.Program.Chart.Colors
  ( blue
  , green
  , black
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils

blue :: GLfloat -> Color4 GLfloat
blue = color4 0 0.25 1 

green :: GLfloat -> Color4 GLfloat
green = color4 0.25 1 0

black :: GLfloat -> Color4 GLfloat
black = color4 0 0 0

