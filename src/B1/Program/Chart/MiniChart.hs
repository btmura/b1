module B1.Program.Chart.MiniChart
  ( MiniChartInput(..)
  , MiniChartOutput(..)
  , MiniChartState(..)
  , drawMiniChart
  , newMiniChartState
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Symbol
import B1.Program.Chart.Resources

data MiniChartInput = MiniChartInput
  { bounds :: Box
  , symbol :: Symbol
  , inputState :: MiniChartState
  }

data MiniChartOutput = MiniChartOutput
  { isDirty :: Bool
  }

data MiniChartState = MiniChartState
  deriving (Show)

newMiniChartState :: MiniChartState
newMiniChartState = MiniChartState

drawMiniChart :: Resources -> MiniChartInput -> IO MiniChartOutput
drawMiniChart resources
    MiniChartInput
      { bounds = bounds
      , symbol = symbol
      } = do
  preservingMatrix $ do
    loadIdentity
    translateToCenter bounds
    color $ blue 1
    drawRoundedRectangle (boxWidth paddedBox) (boxHeight paddedBox)
        cornerRadius cornerVertices

    color $ green 1
    let textSpec = TextSpec (font resources) 10 symbol
    textBounds <- measureText textSpec
    translate $ vector3
        (-(boxWidth bounds / 2 - padding * 2))
        (boxHeight bounds / 2 - padding * 2 - boxHeight textBounds) 0
    renderText textSpec

  return $ MiniChartOutput
    { isDirty = False
    }
  where
    cornerRadius = 5
    cornerVertices = 5
    padding = 5
    paddedBox = boxShrink bounds padding


