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
import qualified B1.Program.Chart.PriceGraph as P
import qualified B1.Program.Chart.StochasticLines as S
import qualified B1.Program.Chart.VolumeBars as V

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
  , volumeBarsState :: V.VolumeBarsState
  , stochasticsState :: S.StochasticLinesState
  , weeklyStochasticsState :: S.StochasticLinesState
  }

newChartState :: Symbol -> IO ChartState
newChartState symbol = do
  stockData <- newStockData symbol
  return ChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.LongStatus H.AddButton
    , priceGraphState = P.newPriceGraphState stockData
    , volumeBarsState = V.newVolumeBarsState stockData
    , stochasticsState = S.newStochasticLinesState stockData dailySpecs
    , weeklyStochasticsState = S.newStochasticLinesState stockData weeklySpecs
    }
  where
    dailySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Daily
        , S.lineColorFunction = red
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Daily
        , S.lineColorFunction = yellow
        , S.stochasticFunction = d
        }
      ]
    weeklySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Weekly
        , S.lineColorFunction = red
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Weekly
        , S.lineColorFunction = purple
        , S.stochasticFunction = d
        }
      ]

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources input =
  convertInputToStuff input
      >>= drawHeader resources
      >>= drawPriceGraph resources
      >>= drawVolumeBars resources
      >>= drawStochasticLines resources
      >>= drawWeeklyStochasticLines resources
      >>= drawHorizontalRules resources
      >>= convertStuffToOutput

data ChartStuff = ChartStuff
  { chartBounds :: Box
  , chartAlpha :: GLfloat
  , chartSymbol :: Symbol
  , chartStockData :: StockData
  , chartHeaderState :: H.HeaderState
  , chartPriceGraphState :: P.PriceGraphState
  , chartVolumeBarsState :: V.VolumeBarsState
  , chartStochasticsState :: S.StochasticLinesState
  , chartWeeklyStochasticsState :: S.StochasticLinesState
  , chartHeaderHeight :: GLfloat
  , chartAddedSymbol :: Maybe Symbol
  , chartIsDirty :: Dirty
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
        , volumeBarsState = volumeBarsState
        , stochasticsState = stochasticsState
        , weeklyStochasticsState = weeklyStochasticsState
        }
      } = 
  return ChartStuff
    { chartBounds = bounds
    , chartAlpha = alpha
    , chartSymbol = symbol
    , chartStockData = stockData
    , chartHeaderState = headerState
    , chartPriceGraphState = priceGraphState
    , chartVolumeBarsState = volumeBarsState
    , chartStochasticsState = stochasticsState
    , chartWeeklyStochasticsState = weeklyStochasticsState
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

  headerOutput <- preservingMatrix $
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

getPriceGraphBounds :: Box -> GLfloat -> Box
getPriceGraphBounds bounds headerHeight = priceGraphBounds
  where
    (priceGraphBounds, _, _, _) = getGraphBounds bounds headerHeight

getVolumeBarsBounds :: Box -> GLfloat -> Box
getVolumeBarsBounds bounds headerHeight = volumeBounds
  where
    (_, volumeBounds, _, _) = getGraphBounds bounds headerHeight

getStochasticsBounds :: Box -> GLfloat -> Box
getStochasticsBounds bounds headerHeight = stochasticsBounds
  where
    (_, _, stochasticsBounds, _) = getGraphBounds bounds headerHeight

getWeeklyStochasticsBounds :: Box -> GLfloat -> Box
getWeeklyStochasticsBounds bounds headerHeight = stochasticsBounds
  where
    (_, _, _, stochasticsBounds) = getGraphBounds bounds headerHeight

getGraphBounds :: Box -> GLfloat -> (Box, Box, Box, Box)
getGraphBounds bounds headerHeight =
  ( priceGraphBounds, volumeBarsBounds,
      stochasticsBounds, weeklyStochasticsBounds)
  where
    Box (left, top) (right, bottom) = bounds
    bottomPadding = 20
    remainingHeight = boxHeight bounds - headerHeight - bottomPadding
    priceGraphHeight = remainingHeight * 0.55
    volumeBarsHeight = (remainingHeight - priceGraphHeight) / 3
    stochasticsHeight = volumeBarsHeight
    weeklyStochasticsHeight = remainingHeight - priceGraphHeight
        - volumeBarsHeight - stochasticsHeight

    priceGraphBounds = Box (left, top - headerHeight)
        (right, top - headerHeight - priceGraphHeight)
    volumeBarsBounds = Box (left, boxBottom priceGraphBounds)
        (right, boxBottom priceGraphBounds - volumeBarsHeight)
    stochasticsBounds = Box (left, boxBottom volumeBarsBounds)
        (right, boxBottom volumeBarsBounds - stochasticsHeight)
    weeklyStochasticsBounds = Box (left, boxBottom stochasticsBounds)
        (right, boxBottom stochasticsBounds - weeklyStochasticsHeight)

-- Starts translating from the center of outerBounds
translateToCenter :: Box -> GLfloat -> Box -> IO ()
translateToCenter outerBounds headerHeight innerBounds =
  translate $ vector3 translateX translateY 0
  where
    translateX = -(boxWidth outerBounds / 2) -- Goto left of outer
        + (boxRight innerBounds - boxLeft outerBounds) -- Goto right of inner 
        - (boxWidth innerBounds / 2) -- Go back half of inner
    translateY = -(boxHeight outerBounds / 2) -- Goto bottom of outer
        + (boxTop innerBounds - boxBottom outerBounds) -- Goto top of inner
        - (boxHeight innerBounds / 2) -- Go down half of inner

drawPriceGraph :: Resources -> ChartStuff -> IO ChartStuff
drawPriceGraph resources
    stuff@ChartStuff
      { chartBounds = bounds
      , chartAlpha = alpha
      , chartPriceGraphState = priceGraphState
      , chartHeaderHeight = headerHeight
      , chartIsDirty = isDirty
      } = do
  let inputBounds = boxShrink (getPriceGraphBounds bounds headerHeight) 1
      priceGraphInput = P.PriceGraphInput
        { P.bounds = inputBounds
        , P.alpha = alpha
        , P.inputState = priceGraphState
        }

  priceGraphOutput <- preservingMatrix $ do
    translateToCenter bounds headerHeight inputBounds
    P.drawPriceGraph resources priceGraphInput

  let P.PriceGraphOutput
        { P.outputState = outputPriceGraphState
        , P.isDirty = isPriceGraphDirty
        } = priceGraphOutput
  return stuff
    { chartPriceGraphState = outputPriceGraphState
    , chartIsDirty = isDirty || isPriceGraphDirty
    }

drawVolumeBars :: Resources -> ChartStuff -> IO ChartStuff
drawVolumeBars resources
    stuff@ChartStuff
      { chartBounds = bounds@(Box (left, top) (right, bottom))
      , chartAlpha = alpha
      , chartVolumeBarsState = volumeBarsState
      , chartHeaderHeight = headerHeight
      , chartIsDirty = isDirty
      } = do
  let inputBounds = boxShrink (getVolumeBarsBounds bounds headerHeight) 1
      volumeBarsInput = V.VolumeBarsInput
        { V.bounds = inputBounds
        , V.alpha = alpha
        , V.inputState = volumeBarsState
        }

  volumeBarsOutput <- preservingMatrix $ do
    translateToCenter bounds headerHeight inputBounds
    V.drawVolumeBars resources volumeBarsInput

  let V.VolumeBarsOutput
        { V.outputState = outputVolumeBarsState
        , V.isDirty = isVolumeDirty
        } = volumeBarsOutput
  return stuff
    { chartVolumeBarsState = outputVolumeBarsState
    , chartIsDirty = isDirty || isVolumeDirty
    }

drawStochasticLines :: Resources -> ChartStuff -> IO ChartStuff
drawStochasticLines resources
    stuff@ChartStuff
      { chartBounds = bounds@(Box (left, top) (right, bottom))
      , chartAlpha = alpha
      , chartStochasticsState = stochasticsState
      , chartHeaderHeight = headerHeight
      , chartIsDirty = isDirty
      } = do
  let inputBounds = boxShrink (getStochasticsBounds bounds headerHeight) 1
      stochasticsInput = S.StochasticLinesInput
        { S.bounds = inputBounds
        , S.alpha = alpha
        , S.inputState = stochasticsState
        }

  stochasticsOutput <- preservingMatrix $ do
    translateToCenter bounds headerHeight inputBounds
    S.drawStochasticLines resources stochasticsInput

  let S.StochasticLinesOutput
        { S.outputState = outputStochasticsState
        , S.isDirty = isStochasticsDirty
        } = stochasticsOutput
  return stuff
    { chartStochasticsState = outputStochasticsState
    , chartIsDirty = isDirty || isStochasticsDirty
    }

drawWeeklyStochasticLines :: Resources -> ChartStuff -> IO ChartStuff
drawWeeklyStochasticLines resources
    stuff@ChartStuff
      { chartBounds = bounds@(Box (left, top) (right, bottom))
      , chartAlpha = alpha
      , chartWeeklyStochasticsState = stochasticsState
      , chartHeaderHeight = headerHeight
      , chartIsDirty = isDirty
      } = do
  let inputBounds = boxShrink (getWeeklyStochasticsBounds bounds headerHeight) 1
      stochasticsInput = S.StochasticLinesInput
        { S.bounds = inputBounds
        , S.alpha = alpha
        , S.inputState = stochasticsState
        }

  stochasticsOutput <- preservingMatrix $ do
    translateToCenter bounds headerHeight inputBounds
    S.drawStochasticLines resources stochasticsInput

  let S.StochasticLinesOutput
        { S.outputState = outputStochasticsState
        , S.isDirty = isStochasticsDirty
        } = stochasticsOutput
  return stuff
    { chartWeeklyStochasticsState = outputStochasticsState
    , chartIsDirty = isDirty || isStochasticsDirty
    }

drawHorizontalRules :: Resources -> ChartStuff -> IO ChartStuff
drawHorizontalRules resources
    stuff@ChartStuff
      { chartBounds = bounds
      , chartAlpha = alpha
      , chartHeaderHeight = headerHeight
      } =
  preservingMatrix $ do
    lineWidth $= 1
    translate $ vector3 0 (boxHeight bounds / 2) 0
    mapM_ (\offset -> do
        translate $ vector3 0 (-offset) 0
        color $ outlineColor resources bounds alpha 
        drawHorizontalRule (boxWidth bounds - 1) 
      ) offsets
    return stuff
  where
    (priceGraphBounds, volumeBounds, stochasticBounds, weeklyStochasticBounds) =
        getGraphBounds bounds headerHeight
    offsets =
      [ headerHeight
      , boxHeight priceGraphBounds
      , boxHeight volumeBounds
      , boxHeight stochasticBounds
      , boxHeight weeklyStochasticBounds
      ]

convertStuffToOutput :: ChartStuff -> IO ChartOutput
convertStuffToOutput 
    ChartStuff
      { chartStockData = stockData
      , chartHeaderState = headerState
      , chartPriceGraphState = priceGraphState
      , chartVolumeBarsState = volumeBarsState
      , chartStochasticsState = stochasticsState
      , chartWeeklyStochasticsState = weeklyStochasticsState
      , chartIsDirty = isDirty
      , chartAddedSymbol = addedSymbol
      } =
  return ChartOutput
    { outputState = ChartState
      { stockData = stockData
      , headerState = headerState
      , priceGraphState = priceGraphState
      , volumeBarsState = volumeBarsState
      , stochasticsState = stochasticsState
      , weeklyStochasticsState = weeklyStochasticsState
      }
    , isDirty = isDirty
    , addedSymbol = addedSymbol
    }

