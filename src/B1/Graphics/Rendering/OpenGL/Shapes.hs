module B1.Graphics.Rendering.OpenGL.Shapes
  ( drawHorizontalRule
  , drawVerticalRule
  , drawRectangle
  , fillRectangle
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Utils

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle = renderRectangle LineLoop

fillRectangle :: GLfloat -> GLfloat -> GLfloat -> IO ()
fillRectangle = renderRectangle Polygon

renderRectangle :: PrimitiveMode -> GLfloat -> GLfloat -> GLfloat -> IO ()
renderRectangle primitiveMode width height padding =
  renderPrimitive primitiveMode $ do
    vertex $ vertex2 left top
    vertex $ vertex2 (right - padding) top
    vertex $ vertex2 right (top - padding)
    vertex $ vertex2 right bottom
    vertex $ vertex2 (left + padding) bottom
    vertex $ vertex2 left (bottom + padding)
  where
    halfWidth = width / 2
    halfHeight = height / 2
    left = -halfWidth
    right = halfWidth
    top = halfHeight
    bottom = -halfHeight

-- | Draws a horizontal rule of width around (0, 0).
drawHorizontalRule :: GLfloat -> IO ()
drawHorizontalRule width =
  renderPrimitive Lines $ do
    vertex $ vertex2 (-width / 2) 0
    vertex $ vertex2 (width / 2) 0

-- | Draws a vertical rule of height around (0, 0).
drawVerticalRule :: GLfloat -> IO ()
drawVerticalRule height =
  renderPrimitive Lines $ do
    vertex $ vertex2 0 (-height / 2)
    vertex $ vertex2 0 (height / 2)

