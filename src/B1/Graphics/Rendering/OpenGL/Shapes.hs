module B1.Graphics.Rendering.OpenGL.Shapes
  ( circleVertex2
  , drawSquarePlaceholder
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils

circleVertex2 :: GLfloat -> Vertex2 GLfloat
circleVertex2 radians = vertex2 (realToFrac (cos radians))
    (realToFrac (sin radians))

drawSquarePlaceholder :: IO ()
drawSquarePlaceholder = do
  renderPrimitive LineLoop $ do
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 1
    vertex $ vertex2 1 (-1)

  renderPrimitive Lines $ do
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 (-1)
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 1 1

