module B1.Graphics.Rendering.OpenGL.Point
  ( Point
  , isLeftOf
  , isRightOf
  , isAbove
  , isBelow
  ) where 

import Graphics.Rendering.OpenGL

type Point = (GLfloat, GLfloat) 

isLeftOf :: Point -> Point -> Bool
isLeftOf (x, _) (otherX, _) = x <= otherX

isRightOf :: Point -> Point -> Bool
isRightOf point otherPoint = not $ isLeftOf point otherPoint

isAbove :: Point -> Point -> Bool
isAbove (_, y) (_, otherY) = y > otherY

isBelow :: Point -> Point -> Bool
isBelow point otherPoint = not $ isAbove point otherPoint

