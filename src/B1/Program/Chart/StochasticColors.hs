module B1.Program.Chart.StochasticColors
  ( getStochasticColors
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Technicals.Stochastic

getStochasticColors :: [Stochastic] -> [Color3 GLfloat]
getStochasticColors = map calculateColor

calculateColor :: Stochastic -> Color3 GLfloat
calculateColor Stochastic { k = k } = Color3 r g b
  where
    lowColor = [1.0, 0.4, 0.0]
    highColor = [0.0, 0.2, 1.0]
    colorInputs = zip3 lowColor highColor $ repeat k
    [r, g, b] = map interpolate colorInputs

interpolate :: (GLfloat, GLfloat, Float) -> GLfloat
interpolate (low, high, percent) = low + (high - low) * realToFrac percent

