module B1.Program.Chart.StochasticNumbers
  ( StochasticNumbersInput(..)
  , drawStochasticNumbers
  , getPreferredWidth
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.FTGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Resources

data StochasticNumbersInput = StochasticNumbersInput
  { bounds :: Box
  , alpha :: GLfloat
  }

padding = 5

getTextSpec :: Resources -> String -> TextSpec
getTextSpec resources = TextSpec (font resources) 10

getPreferredWidth :: Resources -> IO GLfloat
getPreferredWidth resources = do
  textBox <- measureText textSpec
  return $ padding * 2 + boxWidth textBox
  where
    textSpec = getTextSpec resources "30%"

drawStochasticNumbers :: Resources -> StochasticNumbersInput -> IO ()
drawStochasticNumbers resources
    input@StochasticNumbersInput
      { bounds = bounds
      , alpha = alpha
      } = do
  color $ green alpha
  preservingMatrix $ do
    textBox <- measureText $ textSpec "30%"
    translate $ vector3 translateLeft
        (-(boxHeight bounds / 2) - boxHeight textBox / 2) 0

    translate $ vector3 0 thirtyPercentHeight 0
    renderText $ textSpec "30%"

    translate $ vector3 0 fortyPercentHeight 0
    renderText $ textSpec "70%"
  where
    textSpec = getTextSpec resources
    translateLeft = -(boxWidth bounds / 2) + padding
    thirtyPercentHeight = boxHeight bounds * 0.3
    fortyPercentHeight = boxHeight bounds * 0.4

