module B1.Program.Chart.Colors
  ( blue
  , green
  , black
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils

blue :: GLfloat -> Color4 GLfloat
blue alpha = color4 0 0.25 1 alpha

green :: GLfloat -> Color4 GLfloat
green alpha = color4 0.25 1 0 alpha

black :: GLfloat -> Color4 GLfloat
black alpha = color4 0 0 0 alpha

