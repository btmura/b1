module B1.Program.Chart.MiniChart
  ( MiniChartInput(..)
  , MiniChartOutput(..)
  , MiniChartState(..)
  , drawMiniChart
  , newMiniChartState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Symbol
import B1.Program.Chart.Resources

data MiniChartInput = MiniChartInput
  { bounds :: Box
  , symbol :: Symbol
  , inputState :: MiniChartState
  }

data MiniChartOutput = MiniChartOutput
  { outputState :: MiniChartState
  , isDirty :: Bool
  }

data MiniChartState = MiniChartState
  { alphaAnimation :: Animation (GLfloat, Dirty)
  , scaleAnimation :: Animation (GLfloat, Dirty)
  }

newMiniChartState :: MiniChartState
newMiniChartState = MiniChartState
  { alphaAnimation = animateOnce $ linearRange 0 1 10
  , scaleAnimation = animateOnce $ linearRange 1.5 1 10
  }

drawMiniChart :: Resources -> MiniChartInput -> IO MiniChartOutput
drawMiniChart resources
    MiniChartInput
      { bounds = bounds
      , symbol = symbol
      , inputState = inputState
      } = do
  preservingMatrix $ do
    loadIdentity
    translateToCenter bounds
    preservingMatrix $ do
      scale3 scale scale 1

      color $ blue alpha
      drawRoundedRectangle (boxWidth paddedBox) (boxHeight paddedBox)
          cornerRadius cornerVertices

      color $ green alpha
      textBounds <- measureText textSpec
      translate $ vector3
          (-(boxWidth bounds / 2 - padding * 2))
          (boxHeight bounds / 2 - padding * 2 - boxHeight textBounds) 0
      renderText textSpec

  return $ MiniChartOutput
    { isDirty = alphaDirty || scaleDirty
    , outputState = inputState
      { alphaAnimation = next $ alphaAnimation inputState
      , scaleAnimation = next $ scaleAnimation inputState
      }
    }
  where
    (alpha, alphaDirty) = current $ alphaAnimation inputState
    (scale, scaleDirty) = current $ scaleAnimation inputState

    cornerRadius = 5
    cornerVertices = 5
    padding = 5
    paddedBox = boxShrink bounds padding

    textSpec = TextSpec (font resources) 10 symbol

