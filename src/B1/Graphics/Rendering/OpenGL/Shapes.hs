module B1.Graphics.Rendering.OpenGL.Shapes
  ( drawSquarePlaceholder
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils

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

