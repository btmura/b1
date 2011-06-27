module B1.Graphics.Rendering.OpenGL.Shapes
  ( opaqueBubble
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils

opaqueBubble :: GLfloat -> GLfloat -> GLfloat
    -> Color4 GLfloat -> Color4 GLfloat -> IO ()
opaqueBubble width height padding fillColor borderColor = do
  blend $= Disabled
  color fillColor >> fillRectangle width height padding
  color borderColor >> drawRectangle width height padding
  blend $= Enabled

fillRectangle :: GLfloat -> GLfloat -> GLfloat -> IO ()
fillRectangle = renderRectangle Polygon

drawRectangle :: GLfloat -> GLfloat -> GLfloat -> IO ()
drawRectangle = renderRectangle LineLoop

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

