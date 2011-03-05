module B1.Program.Chart.MiniChart
  ( MiniChartInput(..)
  , MiniChartOutput(..)
  , MiniChartState(..)
  , drawMiniChart
  , newMiniChartState
  ) where

import Data.Maybe
import Graphics.UI.GLFW
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
  , alpha :: GLfloat
  , symbol :: Symbol
  , isBeingDragged :: Bool
  , inputState :: MiniChartState
  }

data MiniChartOutput = MiniChartOutput
  { outputState :: MiniChartState
  , isDirty :: Bool
  , symbolRequest :: Maybe Symbol
  , removeChart :: Bool
  }

data MiniChartState = MiniChartState
  { stockData :: StockData
  , headerState :: H.HeaderState
  }

newMiniChartState :: Symbol -> IO MiniChartState
newMiniChartState symbol = do
  stockData <- newStockData symbol
  return $ MiniChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.ShortStatus H.RemoveButton
    }

drawMiniChart :: Resources -> MiniChartInput -> IO MiniChartOutput
drawMiniChart resources
    MiniChartInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , isBeingDragged = isBeingDragged
      , inputState = inputState@MiniChartState
        { stockData = stockData
        , headerState = headerState
        }
      } = do
  preservingMatrix $ do
    color finalColor
    drawRoundedRectangle (boxWidth paddedBox) (boxHeight paddedBox)
        cornerRadius cornerVertices

    let headerInput = H.HeaderInput
          { H.bounds = paddedBox
          , H.fontSize = 10
          , H.padding = 5
          , H.alpha = alpha
          , H.symbol = symbol
          , H.stockData = stockData
          , H.inputState = headerState
          }

    headerOutput <- H.drawHeader resources headerInput

    let H.HeaderOutput
          { H.outputState = outputHeaderState
          , H.height = headerHeight
          , H.clickedSymbol = maybeRemovedSymbol
          , H.isDirty = isHeaderDirty
          } = headerOutput

    translate $ vector3 0 (boxHeight paddedBox / 2 - headerHeight) 0
    color finalColor
    drawHorizontalRule (boxWidth paddedBox - 1)

    let nextRemoveChart = isJust maybeRemovedSymbol
        nextSymbolRequest
          | isNothing maybeRemovedSymbol
              && boxContains paddedBox (mousePosition resources)
              && isMouseButtonClicked resources ButtonLeft = Just symbol
          | otherwise = Nothing
    return $ MiniChartOutput
      { isDirty = isHeaderDirty || isJust nextSymbolRequest || nextRemoveChart
      , symbolRequest = nextSymbolRequest
      , removeChart = nextRemoveChart
      , outputState = inputState
        { headerState = outputHeaderState
        }
      }
  where
    cornerRadius = 5
    cornerVertices = 5
    padding = 5
    paddedBox = boxShrink bounds padding
    finalColor
      | isBeingDragged = gray alpha
      | otherwise = outlineColor resources paddedBox alpha
