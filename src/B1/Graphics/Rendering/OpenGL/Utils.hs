module B1.Graphics.Rendering.OpenGL.Utils
  ( color3
  , scale3
  , texCoord2
  , vector3
  , vertex2
  , vertex3
  ) where

import Graphics.Rendering.OpenGL

color3 :: GLfloat -> GLfloat -> GLfloat -> Color3 GLfloat
color3 = Color3

scale3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
scale3 = scale

texCoord2 :: GLfloat -> GLfloat -> TexCoord2 GLfloat
texCoord2 = TexCoord2

vector3 :: GLfloat -> GLfloat -> GLfloat -> Vector3 GLfloat
vector3 = Vector3

vertex2 :: GLfloat -> GLfloat -> Vertex2 GLfloat
vertex2 = Vertex2

vertex3 :: GLfloat -> GLfloat -> GLfloat -> Vertex3 GLfloat
vertex3 = Vertex3
