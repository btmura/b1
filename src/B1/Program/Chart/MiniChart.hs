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
import B1.Program.Chart.Resources
import B1.Program.Chart.StockData
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.Header as H

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
  { stockData :: StockData
  , headerState :: H.HeaderState
  , alphaAnimation :: Animation (GLfloat, Dirty)
  , scaleAnimation :: Animation (GLfloat, Dirty)
  }

newMiniChartState :: Symbol -> IO MiniChartState
newMiniChartState symbol = do
  stockData <- newStockData symbol
  return $ MiniChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.ShortStatus
    , alphaAnimation = animateOnce $ linearRange 0 1 20
    , scaleAnimation = animateOnce $ linearRange 1 1 20
    }

drawMiniChart :: Resources -> MiniChartInput -> IO MiniChartOutput
drawMiniChart resources
    MiniChartInput
      { bounds = bounds
      , symbol = symbol
      , inputState = inputState@MiniChartState
        { stockData = stockData
        , headerState = headerState
        , alphaAnimation = alphaAnimation
        , scaleAnimation = scaleAnimation
        }
      } = do
  preservingMatrix $ do
    loadIdentity
    translateToCenter bounds
    preservingMatrix $ do
      scale3 scale scale 1

      color $ blue alpha
      drawRoundedRectangle (boxWidth paddedBox) (boxHeight paddedBox)
          cornerRadius cornerVertices

      let headerInput = H.HeaderInput
            { H.bounds = bounds
            , H.fontSize = 10
            , H.alpha = alpha
            , H.symbol = symbol
            , H.stockData = stockData
            , H.inputState = headerState
            }

      headerOutput <- H.drawHeader resources headerInput

      let H.HeaderOutput
            { H.outputState = outputHeaderState
            , H.isDirty = isHeaderDirty
            } = headerOutput

      return $ MiniChartOutput
        { isDirty = alphaDirty || scaleDirty || isHeaderDirty
        , outputState = inputState
          { headerState = outputHeaderState
          , alphaAnimation = next alphaAnimation 
          , scaleAnimation = next scaleAnimation
          }
        }
  where
    (alpha, alphaDirty) = current alphaAnimation 
    (scale, scaleDirty) = current scaleAnimation 

    cornerRadius = 5
    cornerVertices = 5
    padding = 5
    paddedBox = boxShrink bounds padding

