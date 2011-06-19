module B1.Program.Chart.Colors
  ( black4
  , blue4
  , darkBlue3
  , lightBlue4
  , lighterBlue4
  , gray4
  , green3
  , green4
  , red3
  , red4
  , purple3
  , purple4
  , white3
  , white4
  , yellow3
  , yellow4

  , color3ToList
  , outlineColor
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Resources

black4 :: GLfloat -> Color4 GLfloat
black4 = color4 0 0 0

blue4 :: GLfloat -> Color4 GLfloat
blue4 = color4 0 0.25 0.5 

darkBlue3 :: Color3 GLfloat
darkBlue3 = color3 0 0.05 0.15

lightBlue4 :: GLfloat -> Color4 GLfloat
lightBlue4 = color4 0 0.25 0.75

lighterBlue4 :: GLfloat -> Color4 GLfloat
lighterBlue4 = color4 0 0.25 1

gray4 :: GLfloat -> Color4 GLfloat
gray4 = color4 0.5 0.5 0.5

green3 :: Color3 GLfloat
green3 = color3 0.25 1 0

green4 :: GLfloat -> Color4 GLfloat
green4 = color4 0.25 1 0

red3 :: Color3 GLfloat
red3 = color3 1 0.3 0

red4 :: GLfloat -> Color4 GLfloat
red4 = color4 1 0.3 0

purple3 :: Color3 GLfloat
purple3 = color3 0.5 0 1

purple4 :: GLfloat -> Color4 GLfloat
purple4 = color4 0.5 0 1

white3 :: Color3 GLfloat
white3 = color3 1 1 1

white4 :: GLfloat -> Color4 GLfloat
white4 = color4 1 1 1

yellow3 :: Color3 GLfloat
yellow3 = color3 1 1 0

yellow4 :: GLfloat -> Color4 GLfloat
yellow4 = color4 1 1 0

color3ToList :: Color3 GLfloat -> [GLfloat]
color3ToList (Color3 r g b) = r:g:b:[]

outlineColor :: Resources -> Box -> GLfloat -> Color4 GLfloat
outlineColor resources@Resources { mousePosition = mousePosition } bounds
  | clicked = lighterBlue4
  | hover = lightBlue4
  | otherwise = blue4
  where 
    -- TODO: Make helper method for hover and click states.
    hover = boxContains bounds mousePosition
    clicked = hover && isMouseButtonClicked resources ButtonLeft

