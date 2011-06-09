module B1.Program.Chart.Button
  ( ButtonState(..)
  , drawButton
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Resources

data ButtonState = Normal | Hovering | Clicked deriving (Eq, Show)

drawButton :: Resources -> Box -> Int -> GLfloat -> IO ButtonState
drawButton resources bounds textureNumber alpha = do
  let (normalScaleX, normalScaleY) = (boxWidth bounds / 2, boxHeight bounds / 2)
      (hoverScaleX, hoverScaleY) = (normalScaleX * 1.05, normalScaleY * 1.05)
      (clickScaleX, clickScaleY) = (normalScaleX * 0.75, normalScaleY * 0.75)

      hovering = alpha >= 1 && boxContains bounds (mousePosition resources)
      clicked = hovering && isMouseButtonClicked resources ButtonLeft

      (buttonState, buttonColor, scaleX, scaleY)
        | clicked = (Clicked, white4, clickScaleX, clickScaleY)
        | hovering = (Hovering, white4, hoverScaleX, hoverScaleY)
        | otherwise = (Normal, gray4, normalScaleX, normalScaleY)

  preservingMatrix $ do
    color $ buttonColor alpha
    scale3 scaleX scaleY 1
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

