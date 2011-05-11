module B1.Program.Chart.Colors
  ( black
  , blue
  , lightBlue
  , lighterBlue
  , gray
  , green
  , green3
  , red
  , red3
  , purple
  , purple3
  , white
  , white3
  , yellow
  , yellow3

  , color3ToList
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

green3 :: Color3 GLfloat
green3 = color3 0.25 1 0

red :: GLfloat -> Color4 GLfloat
red = color4 1 0.3 0

red3 :: Color3 GLfloat
red3 = color3 1 0.3 0

purple :: GLfloat -> Color4 GLfloat
purple = color4 0.5 0 1

purple3 :: Color3 GLfloat
purple3 = color3 0.5 0 1

white :: GLfloat -> Color4 GLfloat
white = color4 1 1 1

white3 :: Color3 GLfloat
white3 = color3 1 1 1

yellow :: GLfloat -> Color4 GLfloat
yellow = color4 1 1 0

yellow3 :: Color3 GLfloat
yellow3 = color3 1 1 0

color3ToList :: Color3 GLfloat -> [GLfloat]
color3ToList (Color3 r g b) = r:g:b:[]

outlineColor :: Resources -> Box -> GLfloat -> Color4 GLfloat
outlineColor resources@Resources { mousePosition = mousePosition } bounds
  | clicked = lighterBlue
  | hover = lightBlue
  | otherwise = blue
  where 
    -- TODO: Make helper method for hover and click states.
    hover = boxContains bounds mousePosition
    clicked = hover && isMouseButtonClicked resources ButtonLeft

