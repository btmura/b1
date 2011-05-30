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

import qualified B1.Program.Chart.Graph as G
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
  , graphState :: G.GraphState
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
    , graphState = G.newGraphState boundSet
    }
  where
    boundSet = G.GraphBoundSet
      { G.graphBounds = Nothing
      , G.volumeBounds = Nothing
      , G.stochasticsBounds = Just $ Box (-1, 1) (1, 0)
      , G.weeklyStochasticsBounds = Just $ Box (-1, 0) (1, -1)
      }

cleanMiniChartState :: MiniChartState -> IO MiniChartState
cleanMiniChartState state@MiniChartState { graphState = graphState } = do
  newGraphState <- G.cleanGraphState graphState
  return state { graphState = newGraphState }

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
        , graphState = graphState
        }
      } = do
  (newHeaderState, headerDirty, removedSymbol, headerHeight)
      <- drawHeader resources alpha symbol stockData headerState paddedBounds

  (newGraphState, graphDirty) <- preservingMatrix $ do
    let subBounds = Box
            (boxLeft paddedBounds, boxTop paddedBounds - headerHeight)
            (boxRight paddedBounds, boxBottom paddedBounds)
    translate $ vector3 (-(boxWidth paddedBounds / 2))
        (-(boxHeight paddedBounds / 2)) 0
    translate $ vector3 (boxWidth subBounds / 2)
        (boxHeight subBounds / 2) 0
    drawGraph resources alpha stockData graphState subBounds

  drawOutline paddedBounds headerHeight finalColor

  let nextRemoveChart = isJust removedSymbol
      nextSymbolRequest
        | isNothing removedSymbol
            && boxContains paddedBounds (mousePosition resources)
            && isMouseButtonClicked resources ButtonLeft = Just symbol
        | otherwise = Nothing
  return MiniChartOutput
    { isDirty = headerDirty || graphDirty || isJust nextSymbolRequest
        || nextRemoveChart
    , symbolRequest = nextSymbolRequest
    , removeChart = nextRemoveChart
    , outputState = inputState
      { headerState = newHeaderState
      , graphState = newGraphState
      }
    }
  where
    padding = 5
    paddedBounds = boxShrink padding bounds 
    finalColor
      | isBeingDragged = purple4 alpha
      | otherwise = outlineColor resources paddedBounds alpha

drawHeader :: Resources -> GLfloat -> Symbol -> StockData -> H.HeaderState
    -> Box -> IO (H.HeaderState, Dirty, Maybe Symbol, GLfloat)
drawHeader resources alpha symbol stockData headerState bounds = do
  let headerInput = H.HeaderInput
        { H.bounds = bounds
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
  return (outputHeaderState, isHeaderDirty, maybeRemovedSymbol, headerHeight)

drawOutline :: Box -> GLfloat -> Color4 GLfloat -> IO ()
drawOutline bounds headerHeight finalColor = do
  color finalColor
  lineWidth $= 1
  renderPrimitive LineLoop $ do
    vertex $ vertex2 left top
    vertex $ vertex2 (right - headerHeight * slantFactor) top
    vertex $ vertex2 right (top - headerHeight)
    vertex $ vertex2 right bottom
    vertex $ vertex2 left bottom
  renderPrimitive Lines $ do
    vertex $ vertex2 left (top - headerHeight)
    vertex $ vertex2 (right - 1) (top - headerHeight)
    vertex $ vertex2 left stochasticTop
    vertex $ vertex2 (right - 1) stochasticTop
  where
    slantFactor = 0.5
    halfWidth = boxWidth bounds / 2
    halfHeight = boxHeight bounds / 2
    left = -halfWidth
    right = halfWidth
    top = halfHeight
    bottom = -halfHeight
    stochasticTop = top - headerHeight
        - ((boxHeight bounds - headerHeight) / 2)

drawGraph :: Resources -> GLfloat -> StockData -> G.GraphState -> Box
    -> IO (G.GraphState, Dirty)
drawGraph resources alpha stockData graphState bounds = do
  let graphInput = G.GraphInput
        { G.bounds = boxShrink 1 bounds
        , G.alpha = alpha
        , G.stockData = stockData
        , G.inputState = graphState
        }

  graphOutput <- G.drawGraph resources graphInput

  let G.GraphOutput
        { G.outputState = outputGraphState
        , G.isDirty = isGraphDirty
        } = graphOutput
  return (outputGraphState, isGraphDirty)
