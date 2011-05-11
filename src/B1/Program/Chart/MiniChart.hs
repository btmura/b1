module B1.Program.Chart.MiniChart
  ( MiniChartInput(..)
  , MiniChartOutput(..)
  , MiniChartState(symbol)
  , drawMiniChart
  , newMiniChartState
  , cleanMiniChartState
  ) where

import Data.Maybe
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Data.Symbol
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Header as H
import qualified B1.Program.Chart.StochasticLines as S

data MiniChartInput = MiniChartInput
  { bounds :: Box
  , alpha :: GLfloat
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
  { symbol :: Symbol
  , stockData :: StockData
  , headerState :: H.HeaderState
  , stochasticsState :: S.StochasticLinesState
  }

newMiniChartState :: Symbol -> Maybe StockData -> IO MiniChartState
newMiniChartState symbol maybeStockData = do
  stockData <- case maybeStockData of
    Just existingStockData -> return existingStockData
    _ -> newStockData symbol
  return MiniChartState
    { symbol = symbol
    , stockData = stockData
    , headerState = H.newHeaderState H.ShortStatus H.RemoveButton
    , stochasticsState = S.newStochasticLinesState lineSpecs
    }
  where
    lineSpecs =
      [ S.StochasticLineSpec
        { S.timeSpec = S.Daily
        , S.lineColor = yellow3
        , S.stochasticFunction = d
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Weekly
        , S.lineColor = purple3
        , S.stochasticFunction = d
        }
      ]

cleanMiniChartState :: MiniChartState -> IO MiniChartState
cleanMiniChartState
    state@MiniChartState
      { stochasticsState = stochasticsState
      } = do
  newStochasticsState <- S.cleanStochasticLinesState stochasticsState
  return state { stochasticsState = newStochasticsState }

drawMiniChart :: Resources -> MiniChartInput -> IO MiniChartOutput
drawMiniChart resources
    MiniChartInput
      { bounds = bounds
      , alpha = alpha
      , isBeingDragged = isBeingDragged
      , inputState = inputState@MiniChartState
        { symbol = symbol
        , stockData = stockData
        , headerState = headerState
        , stochasticsState = stochasticsState
        }
      } = do
  color finalColor
  lineWidth $= 1
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

  preservingMatrix $ do
    translate $ vector3 0 (boxHeight paddedBox / 2 - headerHeight) 0
    color finalColor
    drawHorizontalRule (boxWidth paddedBox - 1)

  let stochasticsBounds = Box
          (boxLeft paddedBox, boxTop paddedBox - headerHeight)
          (boxRight paddedBox, boxBottom paddedBox)
      stochasticsInput = S.StochasticLinesInput
        { S.bounds = stochasticsBounds
        , S.alpha = alpha
        , S.stockData = stockData
        , S.inputState = stochasticsState
        }

  stochasticsOutput <- preservingMatrix $ do
    translate $ vector3 (-(boxWidth paddedBox / 2))
        (-(boxHeight paddedBox / 2)) 0
    translate $ vector3 (boxWidth stochasticsBounds / 2)
        (boxHeight stochasticsBounds / 2) 0
    S.drawStochasticLines resources stochasticsInput

  let S.StochasticLinesOutput
        { S.outputState = outputStochasticsState
        , S.isDirty = isStochasticsDirty
        } = stochasticsOutput

  let nextRemoveChart = isJust maybeRemovedSymbol
      nextSymbolRequest
        | isNothing maybeRemovedSymbol
            && boxContains paddedBox (mousePosition resources)
            && isMouseButtonClicked resources ButtonLeft = Just symbol
        | otherwise = Nothing
  return MiniChartOutput
    { isDirty = isHeaderDirty
        || isStochasticsDirty
        || isJust nextSymbolRequest
        || nextRemoveChart
    , symbolRequest = nextSymbolRequest
    , removeChart = nextRemoveChart
    , outputState = inputState
      { headerState = outputHeaderState
      , stochasticsState = outputStochasticsState
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
