module B1.Program.Chart.Button
  ( ButtonState(..)
  , drawButton
  ) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Resources

data ButtonState = Normal | Hovering | Clicked deriving (Eq)

drawButton :: Resources -> Box -> Int -> GLfloat -> IO ButtonState
drawButton resources bounds textureNumber alpha = 
  preservingMatrix $ do
    color $ buttonColor alpha
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just (TextureObject
        (fromIntegral textureNumber))
    renderPrimitive Quads $ do
      normal $ normal3 0 0 1
      texCoord $ texCoord2 0 0
      vertex $ vertex2 (-1) (-1)
      texCoord $ texCoord2 0 1
      vertex $ vertex2 (-1) 1
      texCoord $ texCoord2 1 1
      vertex $ vertex2 1 1
      texCoord $ texCoord2 1 0
      vertex $ vertex2 1 (-1)
    texture Texture2D $= Disabled
    return buttonState
  where
    hovering = alpha >= 1 && boxContains bounds (mousePosition resources)
    clicked = hovering && isMouseButtonClicked resources ButtonLeft
    buttonColor = if hovering || clicked then white4 else gray4
    buttonState
      | clicked = Clicked
      | hovering = Hovering
      | otherwise = Normal

