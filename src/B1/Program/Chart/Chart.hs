module B1.Program.Chart.Chart
  ( ChartInput(..)
  , ChartOutput(..)
  , ChartState(stockData)
  , drawChart
  , newChartState
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

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
import qualified B1.Program.Chart.PriceGraph as P

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
  , priceGraphState :: P.PriceGraphState
  }

newChartState :: Symbol -> IO ChartState
newChartState symbol = do
  stockData <- newStockData symbol
  return $ ChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.LongStatus H.AddButton
    , priceGraphState = P.newPriceGraphState
    }

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources input =
  convertInputToStuff input
      >>= drawHeader resources
      >>= drawPriceGraph resources
      >>= drawHorizontalRules resources
      >>= convertStuffToOutput

data ChartStuff = ChartStuff
  { chartBounds :: Box
  , chartAlpha :: GLfloat
  , chartSymbol :: Symbol
  , chartStockData :: StockData
  , chartHeaderState :: H.HeaderState
  , chartPriceGraphState :: P.PriceGraphState
  , chartHeaderHeight :: GLfloat
  , chartAddedSymbol :: Maybe Symbol
  , chartIsDirty :: Bool
  }

convertInputToStuff :: ChartInput -> IO ChartStuff
convertInputToStuff 
    ChartInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , inputState = ChartState
        { stockData = stockData
        , headerState = headerState
        , priceGraphState = priceGraphState
        }
      } = 
  return ChartStuff
    { chartBounds = bounds
    , chartAlpha = alpha
    , chartSymbol = symbol
    , chartStockData = stockData
    , chartHeaderState = headerState
    , chartPriceGraphState = priceGraphState
    , chartHeaderHeight = 0
    , chartAddedSymbol = Nothing
    , chartIsDirty = False
    }

drawHeader :: Resources -> ChartStuff -> IO ChartStuff
drawHeader resources
    stuff@ChartStuff
      { chartBounds = bounds
      , chartAlpha = alpha
      , chartSymbol = symbol
      , chartStockData = stockData
      , chartHeaderState = headerState
      , chartIsDirty = isDirty
      } = do

  let headerInput = H.HeaderInput
        { H.bounds = bounds
        , H.fontSize = 18
        , H.padding = 10
        , H.alpha = alpha
        , H.symbol = symbol
        , H.stockData = stockData
        , H.inputState = headerState
        }

  headerOutput <- preservingMatrix $ do
    H.drawHeader resources headerInput 

  let H.HeaderOutput
        { H.outputState = outputHeaderState
        , H.isDirty = isHeaderDirty
        , H.height = headerHeight
        , H.clickedSymbol = addedSymbol
        } = headerOutput
  return stuff
    { chartHeaderState = outputHeaderState
    , chartHeaderHeight = headerHeight
    , chartIsDirty = isDirty || isHeaderDirty
    , chartAddedSymbol = addedSymbol
    }

drawPriceGraph :: Resources -> ChartStuff -> IO ChartStuff
drawPriceGraph resources
    stuff@ChartStuff
      { chartBounds = bounds@(Box (left, top) (right, bottom))
      , chartAlpha = alpha
      , chartPriceGraphState = priceGraphState
      , chartHeaderHeight = headerHeight
      , chartIsDirty = isDirty
      } = do
  let inputBounds = Box (left, top - headerHeight) (right, bottom)
      priceGraphInput = P.PriceGraphInput
        { P.bounds = inputBounds
        , P.alpha = alpha
        , P.inputState = priceGraphState
        }

  priceGraphOutput <- preservingMatrix $ do
    translate $ vector3 (-(boxWidth bounds / 2)) (-(boxHeight bounds / 2)) 0
    translate $ vector3 (boxWidth inputBounds / 2) (boxHeight inputBounds / 2) 0
    P.drawPriceGraph resources priceGraphInput

  let P.PriceGraphOutput
        { P.outputState = outputPriceGraphState
        , P.isDirty = isPriceGraphDirty
        } = priceGraphOutput
  return stuff
    { chartPriceGraphState = outputPriceGraphState
    , chartIsDirty = isDirty || isPriceGraphDirty
    }

drawHorizontalRules :: Resources -> ChartStuff -> IO ChartStuff
drawHorizontalRules resources
    stuff@ChartStuff
      { chartBounds = bounds
      , chartAlpha = alpha
      , chartHeaderHeight = headerHeight
      } = do
  preservingMatrix $ do
    translate $ vector3 0 (boxHeight bounds / 2 - headerHeight) 0
    color $ outlineColor resources bounds alpha 
    drawHorizontalRule (boxWidth bounds - 1) 
  return stuff

convertStuffToOutput :: ChartStuff -> IO ChartOutput
convertStuffToOutput 
    ChartStuff
      { chartStockData = stockData
      , chartHeaderState = headerState
      , chartPriceGraphState = priceGraphState
      , chartIsDirty = isDirty
      , chartAddedSymbol = addedSymbol
      } =
  return ChartOutput
    { outputState = ChartState
      { stockData = stockData
      , headerState = headerState
      , priceGraphState = priceGraphState
      }
    , isDirty = isDirty
    , addedSymbol = addedSymbol
    }

