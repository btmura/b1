module B1.Program.Chart.FragmentShader
  ( setAlpha
  ) where

import Graphics.Rendering.OpenGL

setAlpha :: Program -> GLfloat -> IO ()
setAlpha program alpha = do
  alphaLocation <- get $ uniformLocation program "alpha" 
  uniform alphaLocation $= Color4 0 0 0 alpha
