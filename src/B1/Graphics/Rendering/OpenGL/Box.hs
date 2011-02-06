module B1.Graphics.Rendering.OpenGL.Box
  ( Box(..)
  , boxCenter
  , boxContains
  , boxShrink
  , boxWidth
  , boxHeight
  , zeroBox
  , zeroBoxHeight
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Point

data Box = 
  -- | Construct a box from upper left and bottom right points.  
  Box Point Point 
  deriving (Show)

boxCenter :: Box -> Point
boxCenter box@(Box (left, top) _) = (centerX, centerY)
  where
    centerX = left + boxWidth box / 2
    centerY = top - boxHeight box / 2

boxContains :: Box -> Point -> Bool
boxContains box@(Box (left, top) (right, bottom)) (x, y) =
  x >= left && x <= right && y >= top && y <= bottom

boxShrink :: Box -> GLfloat -> Box
boxShrink (Box (left, top) (right, bottom)) shrink =
  Box (left + shrink, top + shrink) (right - shrink, bottom - shrink)

boxWidth :: Box -> GLfloat
boxWidth (Box (left, _) (right, _)) = abs $ right - left

boxHeight :: Box -> GLfloat
boxHeight (Box (_, top) (_, bottom)) = abs $ bottom - top

zeroBox :: Box
zeroBox = Box (0, 0) (0, 0)

zeroBoxHeight :: Box -> Box
zeroBoxHeight (Box (left, _) (right, _)) = Box (left, 0) (right, 0)
