module B1.Program.Chart.Chart
  ( ChartInput(..)
  , ChartOutput(..)
  , ChartState(stockData)
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
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Graph as G
import qualified B1.Program.Chart.GraphNumbers as GN
import qualified B1.Program.Chart.Header as H
import qualified B1.Program.Chart.StochasticNumbers as SN

data ChartInput = ChartInput
  { bounds :: Box
  , alpha :: GLfloat
  , symbol :: Symbol
  , inputState :: ChartState
  }

data ChartOutput = ChartOutput
  { outputState :: ChartState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol
  }

data ChartState = ChartState
  { stockData :: StockData
  , headerState :: H.HeaderState
  , graphState :: G.GraphState
  }

newChartState :: Symbol -> IO ChartState
newChartState symbol = do
  stockData <- newStockData symbol
  return ChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.LongStatus H.AddButton
    , graphState = G.newGraphState boundSet
    }
  where
    boundSet = G.GraphBoundSet
      { G.graphBounds = Just $ Box (-1, 1) (1, -0.1)
      , G.volumeBounds = Just $ Box (-1, -0.1) (1, -0.4)
      , G.stochasticsBounds = Just $ Box (-1, -0.4) (1, -0.7)
      , G.weeklyStochasticsBounds = Just $ Box (-1, -0.7) (1, -1)
      }

cleanChartState :: ChartState -> IO ChartState
cleanChartState state@ChartState { graphState = graphState } = do
  newGraphState <- G.cleanGraphState graphState
  return state { graphState = newGraphState }

bottomPadding = 20

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources
    input@ChartInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , inputState = inputState@ChartState
        { stockData = stockData
        , headerState = headerState
        , graphState = graphState
        }
      } = do
  (newHeaderState, headerDirty, addedSymbol, headerHeight)
      <- drawHeader resources alpha symbol stockData headerState bounds
  boundsSet <- getBounds resources bounds headerHeight stockData

  (newGraphState, graphDirty) <- preservingMatrix $ do
    let subBounds = graphBounds boundsSet
    translateToCenter bounds subBounds
    drawGraph resources alpha stockData graphState subBounds

  preservingMatrix $ do
    let subBounds = graphNumbersBounds boundsSet
    translateToCenter bounds subBounds
    drawGraphNumbers resources alpha GN.Prices stockData subBounds

  preservingMatrix $ do
    let subBounds = volumeBarNumbersBounds boundsSet
    translateToCenter bounds subBounds
    drawGraphNumbers resources alpha GN.Volume stockData subBounds

  preservingMatrix $ do
    let subBounds = stochasticNumbersBounds boundsSet
    translateToCenter bounds subBounds
    drawStochasticNumbers resources alpha subBounds

  preservingMatrix $ do
    let subBounds = weeklyStochasticNumbersBounds boundsSet
    translateToCenter bounds subBounds
    drawStochasticNumbers resources alpha subBounds

  preservingMatrix $ do
    drawDividerLines resources alpha bounds boundsSet headerHeight

  return ChartOutput
    { outputState = inputState
      { headerState = newHeaderState
      , graphState = newGraphState
      }
    , isDirty = headerDirty || graphDirty
    , addedSymbol = addedSymbol
    }

drawHeader :: Resources -> GLfloat -> Symbol -> StockData -> H.HeaderState
    -> Box -> IO (H.HeaderState, Dirty, Maybe Symbol, GLfloat)
drawHeader resources alpha symbol stockData headerState bounds = do
  let headerInput = H.HeaderInput
        { H.bounds = bounds
        , H.fontSize = 18
        , H.padding = 10
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
  { graphBounds :: Box
  , graphNumbersBounds :: Box
  , volumeBarNumbersBounds :: Box
  , stochasticNumbersBounds :: Box
  , weeklyStochasticNumbersBounds :: Box
  }

-- TODO: Move code to Graph
getBounds :: Resources -> Box -> GLfloat -> StockData -> IO Bounds
getBounds resources bounds headerHeight stockData = do
  graphNumbersWidth <- GN.getPreferredWidth resources GN.Prices stockData
  volumeBarNumbersWidth <- GN.getPreferredWidth resources GN.Volume stockData
  stochasticNumbersWidth <- SN.getPreferredWidth resources
  let minNumbersWidth = maximum
        [ graphNumbersWidth
        , volumeBarNumbersWidth
        , stochasticNumbersWidth
        ]
      numbersLeft = right - minNumbersWidth

      Box (left, top) (right, bottom) = bounds
      remainingHeight = boxHeight bounds - headerHeight - bottomPadding
      graphHeight = remainingHeight * 0.55
      volumeBarsHeight = remainingHeight * 0.15
      stochasticsHeight = remainingHeight * 0.15
      weeklyStochasticsHeight = remainingHeight * 0.15

      graphBounds = Box (left, top - headerHeight)
          (numbersLeft, top - headerHeight - graphHeight - volumeBarsHeight
              - stochasticsHeight - weeklyStochasticsHeight)
      graphNumbersBounds = Box
          (numbersLeft, top - headerHeight)
          (right, top - headerHeight - graphHeight)

      volumeBarsBounds = Box (left, top - headerHeight - graphHeight)
          (numbersLeft, top - headerHeight - graphHeight - volumeBarsHeight)
      volumeBarNumbersBounds = Box
          (numbersLeft, boxTop volumeBarsBounds)
          (right, boxBottom volumeBarsBounds)

      stochasticsBounds = Box
          (left, boxBottom volumeBarsBounds)
          (numbersLeft, boxBottom volumeBarsBounds - stochasticsHeight)
      stochasticNumbersBounds = Box
          (numbersLeft, boxTop stochasticsBounds)
          (right, boxBottom stochasticsBounds)

      weeklyStochasticsBounds = Box
          (left, boxBottom stochasticsBounds)
          (numbersLeft, boxBottom stochasticsBounds - weeklyStochasticsHeight)
      weeklyStochasticNumbersBounds = Box
          (numbersLeft, boxTop weeklyStochasticsBounds)
          (right, boxBottom weeklyStochasticsBounds)

  return Bounds
    { graphBounds = graphBounds
    , graphNumbersBounds = graphNumbersBounds
    , volumeBarNumbersBounds = volumeBarNumbersBounds
    , stochasticNumbersBounds = stochasticNumbersBounds
    , weeklyStochasticNumbersBounds = weeklyStochasticNumbersBounds
    }

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

drawGraphNumbers :: Resources -> GLfloat -> GN.GraphNumbersType -> StockData
    -> Box -> IO ()
drawGraphNumbers resources alpha numbersType stockData bounds = do
  let numbersInput = GN.GraphNumbersInput
        { GN.bounds = bounds
        , GN.alpha = alpha
        , GN.stockData = stockData
        , GN.numbersType = numbersType
        }
  GN.drawGraphNumbers resources numbersInput

drawStochasticNumbers :: Resources -> GLfloat -> Box -> IO ()
drawStochasticNumbers resources alpha bounds = do
  let numbersInput = SN.StochasticNumbersInput
        { SN.bounds = bounds
        , SN.alpha = alpha
        }
  SN.drawStochasticNumbers resources numbersInput

drawDividerLines :: Resources -> GLfloat -> Box -> Bounds -> GLfloat -> IO ()
drawDividerLines resources alpha bounds
    Bounds
      { graphBounds = graphBounds
      , volumeBarNumbersBounds = volumeBarNumbersBounds
      , stochasticNumbersBounds = stochasticNumbersBounds
      , weeklyStochasticNumbersBounds = weeklyStochasticNumbersBounds
      }
    headerHeight = do
  lineWidth $= 1
  color $ outlineColor resources bounds alpha

  preservingMatrix $ do
    translate $ vector3 0 translateTop 0
    translateDrawHorizontalRule $ -headerHeight

  preservingMatrix $ do
    translate $ vector3 0 translateBottom 0
    translateDrawHorizontalRule 0
    translateDrawHorizontalRule $ boxHeight weeklyStochasticNumbersBounds
    translateDrawHorizontalRule $ boxHeight stochasticNumbersBounds
    translateDrawHorizontalRule $ boxHeight volumeBarNumbersBounds

  preservingMatrix $ do
    translateToCenter bounds verticalBounds
    drawVerticalRule verticalHeight

  where
    translateTop = boxHeight bounds / 2
    translateBottom = -(boxHeight bounds / 2) + bottomPadding

    translateDrawHorizontalRule :: GLfloat -> IO ()
    translateDrawHorizontalRule yTranslate = do
      translate $ vector3 0 yTranslate 0
      drawHorizontalRule $ boxWidth bounds - 1

    verticalCenter = boxLeft graphBounds + boxWidth graphBounds
    verticalBounds = Box (verticalCenter, boxTop graphBounds)
        (verticalCenter, boxBottom graphBounds)

    verticalHeight = sum $ map boxHeight [graphBounds]
    verticalXOffset = -(boxWidth bounds / 2) + boxWidth graphBounds
    verticalYOffset = boxHeight bounds / 2 - headerHeight - verticalHeight / 2

