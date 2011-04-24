module B1.Program.Chart.PriceGraph
  ( PriceGraphInput(..)
  , PriceGraphOutput(..)
  , PriceGraphState
  , drawPriceGraph
  , newPriceGraphState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.StockData

data PriceGraphInput = PriceGraphInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: PriceGraphState
  }

data PriceGraphOutput = PriceGraphOutput
  { outputState :: PriceGraphState
  , isDirty :: Dirty
  }

data PriceGraphState = PriceGraphState
  { stockData :: StockData
  }

newPriceGraphState :: StockData -> PriceGraphState
newPriceGraphState stockData =
  PriceGraphState
    { stockData = stockData
    }

drawPriceGraph :: Resources -> PriceGraphInput -> IO PriceGraphOutput
drawPriceGraph resources input = 
  convertInputToStuff input
      >>= renderPriceGraph
      >>= convertStuffToOutput

data PriceGraphStuff = PriceGraphStuff
  { priceBounds :: Box
  , priceAlpha :: GLfloat
  , priceStockData :: StockData
  , priceIsDirty :: Dirty
  }

convertInputToStuff :: PriceGraphInput -> IO PriceGraphStuff
convertInputToStuff
    PriceGraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = PriceGraphState
        { stockData = stockData
        }
      } =
  return PriceGraphStuff
    { priceBounds = bounds
    , priceAlpha = alpha
    , priceStockData = stockData
    , priceIsDirty = False
    }

renderPriceGraph :: PriceGraphStuff -> IO PriceGraphStuff
renderPriceGraph
    stuff@PriceGraphStuff
      { priceStockData = stockData
      } = do
  maybePriceData <- getStockPriceData stockData
  maybe (return stuff { priceIsDirty = True })
      (\priceData -> either
          (renderGraph stuff)
          (renderError stuff)
          (pricesOrError priceData))
      maybePriceData

renderGraph :: PriceGraphStuff -> [Price] -> IO PriceGraphStuff
renderGraph
    stuff@PriceGraphStuff
      { priceBounds = bounds
      , priceAlpha = alpha
      }
    prices = do
  lineWidth $= 1
  mapM_ (renderBar alpha) $ getBars bounds prices
  return stuff { priceIsDirty = False }

data Bar = Bar
  { translateX :: GLfloat
  , translateY :: GLfloat
  , barWidth :: GLfloat
  , barHeight :: GLfloat
  , barColor :: GLfloat -> Color4 GLfloat
  , openPercentage :: Float
  , closePercentage :: Float
  }

renderBar :: GLfloat -> Bar -> IO ()
renderBar alpha bar =
  preservingMatrix $ do
    color $ barColor bar alpha
    translate $ vector3 (translateX bar) (translateY bar) 0
    scale3 (barWidth bar / 2) (barHeight bar / 2) 1
    renderPrimitive Lines $ do
      vertex $ vertex2 0 (-1)
      vertex $ vertex2 0 1

      vertex $ vertex2 0 openY
      vertex $ vertex2 (-1) openY

      vertex $ vertex2 0 closeY
      vertex $ vertex2 1 closeY
  where
    openY = -1 + realToFrac (openPercentage bar) * 2
    closeY = -1 + realToFrac (closePercentage bar) * 2

getBars :: Box -> [Price] -> [Bar]
getBars bounds prices = map (createBar bounds prices) [0 .. length prices - 1]

createBar :: Box -> [Price] -> Int -> Bar
createBar bounds prices index = Bar
  { translateX = translateX
  , translateY = translateY
  , barWidth = barWidth
  , barHeight = barHeight
  , barColor = barColor
  , openPercentage = openPercentage
  , closePercentage = closePercentage
  }
  where
    rightPadding = 0
    numPrices = length prices
    barWidth = (boxWidth bounds - rightPadding) / realToFrac numPrices

    -- 1. Move all the way to the right edge.
    -- 2. Move left to account for right padding.
    -- 3. Move left some number of bars.
    -- 4. Move left half a bar to center the bar.
    translateX = boxWidth bounds / 2
        - rightPadding
        - barWidth * realToFrac index
        - barWidth / 2

    maxPrice = maximum $ map high prices
    minPrice = minimum $ map low prices
    totalRange = maxPrice - minPrice

    topPadding = 10
    bottomPadding = 10
    availableHeight = boxHeight bounds - topPadding - bottomPadding

    price = prices !! index
    priceRange = high price - low price
    heightPercentage = priceRange / totalRange
    barHeight = boxHeight bounds * realToFrac heightPercentage
 
    lowerRange = low price - minPrice
    lowerHeightPercentage = lowerRange / totalRange
    lowerHeight = availableHeight * realToFrac lowerHeightPercentage

    -- 1. Move all the way to the bottom.
    -- 2. Move up to account for bottom padding.
    -- 3. Move up to the bottom of the bar.
    -- 4. Move up half a bar to center the bar.
    translateY = -(boxHeight bounds / 2)
        + bottomPadding
        + lowerHeight
        + barHeight / 2

    lowPrice = low price
    highPrice = highPrice
    openPrice = open price
    closePrice = close price

    openPercentage = (openPrice - lowPrice) / priceRange
    closePercentage = (closePrice - lowPrice) / priceRange

    barColor = if getPriceChange prices index >= 0 then green else red

renderError :: PriceGraphStuff -> String -> IO PriceGraphStuff
renderError stuff error = return stuff { priceIsDirty = False }

convertStuffToOutput :: PriceGraphStuff -> IO PriceGraphOutput
convertStuffToOutput
    PriceGraphStuff
      { priceStockData = stockData
      , priceIsDirty = isDirty
      } =
  return PriceGraphOutput
    { outputState = PriceGraphState
      { stockData = stockData
      }
    , isDirty = isDirty
    }

