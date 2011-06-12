module B1.Program.Chart.Chart
  ( ChartInput(..)
  , ChartOutput(..)
  , ChartOptions(..)
  , ChartState(symbol, stockData)
  , drawChart
  , newChartState
  , cleanChartState
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Range
import B1.Data.Symbol
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.LineSegment
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Button
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Graph as G
import qualified B1.Program.Chart.Header as H

data ChartInput = ChartInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: ChartState
  }

data ChartOutput = ChartOutput
  { outputState :: ChartState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol
  , refreshedSymbol :: Maybe Symbol
  }

data ChartOptions = ChartOptions
  { headerOptions :: H.HeaderOptions
  , graphOptions :: G.GraphOptions
  , showRefreshButton :: Bool
  }

data ChartState = ChartState
  { options :: ChartOptions
  , symbol :: Symbol
  , stockData :: StockData
  , headerState :: H.HeaderState
  , graphState :: G.GraphState
  }

newChartState :: ChartOptions -> Symbol -> IO ChartState
newChartState
    options@ChartOptions
      { headerOptions = headerOptions
      , graphOptions = graphOptions
      }
    symbol = do
  stockData <- newStockData symbol
  return ChartState
    { options = options
    , symbol = symbol
    , stockData = stockData
    , headerState = H.newHeaderState headerOptions
    , graphState = G.newGraphState graphOptions stockData
    }

cleanChartState :: ChartState -> IO ChartState
cleanChartState state@ChartState { graphState = graphState } = do
  newGraphState <- G.cleanGraphState graphState
  return state { graphState = newGraphState }

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources
    input@ChartInput
      { bounds = bounds
      , alpha = alpha
      , inputState = inputState@ChartState
        { options = ChartOptions
          { showRefreshButton = showRefreshButton
          }
        , symbol = symbol
        , stockData = stockData
        , headerState = headerState
        , graphState = graphState
        }
      } = do

  (newHeaderState, headerDirty, addedSymbol, headerHeight)
      <- drawHeader resources alpha symbol stockData headerState bounds
  boundsSet <- getBounds resources bounds headerHeight stockData

  refreshClicked <- preservingMatrix $ do
    if showRefreshButton
      then do
        let subBounds = refreshButtonBounds boundsSet
        translateToCenter bounds subBounds
        drawRefreshButton resources subBounds alpha
      else
        return False

  (newGraphState, graphDirty) <- preservingMatrix $ do
    let subBounds = chartBounds boundsSet
    translateToCenter bounds subBounds
    drawGraph resources alpha stockData graphState subBounds

  drawFrame resources bounds headerHeight alpha showRefreshButton

  let nextRefreshedSymbol = if refreshClicked then Just symbol else Nothing
  return ChartOutput
    { outputState = inputState
      { headerState = newHeaderState
      , graphState = newGraphState
      }
    , isDirty = headerDirty || graphDirty
    , addedSymbol = addedSymbol
    , refreshedSymbol = nextRefreshedSymbol
    }

drawHeader :: Resources -> GLfloat -> Symbol -> StockData -> H.HeaderState
    -> Box -> IO (H.HeaderState, Dirty, Maybe Symbol, GLfloat)
drawHeader resources alpha symbol stockData headerState bounds = do
  let headerInput = H.HeaderInput
        { H.bounds = bounds
        , H.alpha = alpha
        , H.symbol = symbol
        , H.stockData = stockData
        , H.inputState = headerState
        }

  headerOutput <- H.drawHeader resources headerInput 

  let H.HeaderOutput
        { H.outputState = outputHeaderState
        , H.isDirty = isHeaderDirty
        , H.height = headerHeight
        , H.clickedSymbol = addedSymbol
        } = headerOutput
  return (outputHeaderState, isHeaderDirty, addedSymbol, headerHeight)

data Bounds = Bounds
  { refreshButtonBounds :: Box
  , chartBounds :: Box
  , priceBounds :: Box
  , volumeBounds :: Box
  , stochasticBounds :: Box
  }

getBounds :: Resources -> Box -> GLfloat -> StockData -> IO Bounds
getBounds resources bounds headerHeight stockData = do
  let Box (left, top) (right, bottom) = bounds
      headerBottom = top - headerHeight
      chartBounds = Box (left, headerBottom) (right, bottom)

      remainingHeight = boxHeight bounds - headerHeight
      graphHeight = remainingHeight * 0.55
      volumeHeight = remainingHeight * 0.15
      stochasticsHeight = remainingHeight * 0.15
      weeklyStochasticsHeight = remainingHeight * 0.15

      priceBounds = Box (left, headerBottom)
          (right, headerBottom - graphHeight)
      volumeBounds = Box (left, boxBottom priceBounds)
          (right, boxBottom priceBounds - volumeHeight)
      stochasticsBounds = Box (left, boxBottom volumeBounds)
          (right, boxBottom volumeBounds - stochasticsHeight)
      weeklyStochasticsBounds = Box (left, boxBottom stochasticsBounds)
          (right, boxBottom stochasticsBounds - weeklyStochasticsHeight)

      refreshButtonBounds = Box (right - headerHeight, top)
          (right, top - headerHeight)

  return Bounds
    { refreshButtonBounds = refreshButtonBounds
    , chartBounds = chartBounds
    , priceBounds = priceBounds
    , volumeBounds = volumeBounds
    , stochasticBounds = stochasticsBounds
    }

drawRefreshButton :: Resources -> Box -> GLfloat -> IO Bool
drawRefreshButton resources bounds alpha = do
  buttonState <- drawButton resources bounds 2 alpha
  return $ buttonState == Clicked

-- Starts translating from the center of outerBounds
translateToCenter :: Box -> Box -> IO ()
translateToCenter outerBounds innerBounds =
  translate $ vector3 translateX translateY 0
  where
    translateX = -(boxWidth outerBounds / 2) -- Goto left of outer
        + (boxRight innerBounds - boxLeft outerBounds) -- Goto right of inner 
        - (boxWidth innerBounds / 2) -- Go back half of inner
    translateY = -(boxHeight outerBounds / 2) -- Goto bottom of outer
        + (boxTop innerBounds - boxBottom outerBounds) -- Goto top of inner
        - (boxHeight innerBounds / 2) -- Go down half of inner

drawGraph :: Resources -> GLfloat -> StockData -> G.GraphState -> Box
    -> IO (G.GraphState, Dirty)
drawGraph resources alpha stockData graphState bounds = do
  let graphInput = G.GraphInput
        { G.bounds = bounds
        , G.alpha = alpha
        , G.inputState = graphState
        }

  graphOutput <- G.drawGraph resources graphInput

  let G.GraphOutput
        { G.outputState = outputGraphState
        , G.isDirty = isGraphDirty
        } = graphOutput
  return (outputGraphState, isGraphDirty)

drawFrame :: Resources -> Box -> GLfloat -> GLfloat -> Bool -> IO ()
drawFrame resources bounds headerHeight alpha showRefreshButton = do
  lineWidth $= 1
  color $ outlineColor resources bounds alpha

  let halfWidth = boxWidth bounds / 2
      halfHeight = boxHeight bounds / 2
      (left, right) = (-halfWidth, halfWidth)
      (top, bottom) = (halfHeight, -halfHeight)

      indentFactor = if showRefreshButton then 1 else 0
      rightForTop = right - (headerHeight / 2) - headerHeight * indentFactor
      rightForBelowHeader = right - headerHeight * indentFactor
      topBelowHeader = top - headerHeight

  -- Render the outermost lines of the jagged frame
  renderPrimitive LineLoop $ do
    vertex $ vertex2 left top
    vertex $ vertex2 rightForTop top
    vertex $ vertex2 rightForBelowHeader topBelowHeader
    vertex $ vertex2 right topBelowHeader
    vertex $ vertex2 right bottom
    vertex $ vertex2 left bottom

  -- Line below the header
  renderPrimitive Lines $ do
    vertex $ vertex2 left topBelowHeader
    vertex $ vertex2 (rightForBelowHeader - 1) topBelowHeader

