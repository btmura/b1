module B1.Program.Chart.Streak
  ( getStreakElements
  , getStreakSize
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Program.Chart.Colors

getStreakSize :: Int
getStreakSize = size
  where
    numQuads = 1
    verticesPerQuad = 4
    floatsPerVertex = 2 + 3 -- x, y, and 3 for color
    size = numQuads * (verticesPerQuad * floatsPerVertex)

getStreakElements :: Box -> Color3 GLfloat -> [GLfloat]
getStreakElements bounds streakColor = elements
  where
    color = color3ToList streakColor
    Box (left, top) (right, bottom) = bounds
    elements =
      -- Top Quad
      [ left
      , bottom
      , 0, 0, 0

      , left
      , top
      , 0, 0, 0

      , right
      , top
      , 0, 0, 0

      , right
      , bottom
      ] ++ color ++

      []

