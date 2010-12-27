module B1.Graphics.Rendering.OpenGL.Utils
  ( scale3
  , rotate4
  , vertex2
  , texCoord2
  ) where

import Graphics.Rendering.OpenGL

-- | Shortcut for scale using GLfloat arguments.
scale3 :: GLfloat -> GLfloat -> GLfloat -> IO ()
scale3 = scale

-- | Shortcut for rotate using GLfloat arguments.
rotate4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
rotate4 c x y z = rotate c $ Vector3 x y z

-- | Shortcut for using Vertex2 using GLfloat arguments.
vertex2 :: GLfloat -> GLfloat -> IO () 
vertex2 x y = vertex $ Vertex2 x y

-- | Shortcut for using TexCoord2 using GLfloat arguments.
texCoord2 :: GLfloat -> GLfloat -> IO ()
texCoord2 x y = texCoord $ TexCoord2 x y
