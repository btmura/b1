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
    prices =
  preservingMatrix $ do
    mapM_ (renderBar alpha) $ getBars bounds prices
    return stuff { priceIsDirty = False }

data Bar = Bar
  { translateX :: GLfloat
  , translateY :: GLfloat
  , barWidth :: GLfloat
  , barHeight :: GLfloat
  }

renderBar :: GLfloat -> Bar -> IO ()
renderBar alpha bar =
  preservingMatrix $ do
    translate $ vector3 (translateX bar) (translateY bar) 0
    scale3 (barWidth bar / 2) (barHeight bar / 2) 1
    renderPrimitive Quads $ do
      vertex $ vertex2 (-1) (-1)
      vertex $ vertex2 (-1) 1
      vertex $ vertex2 1 1
      vertex $ vertex2 1 (-1)

getBars :: Box -> [Price] -> [Bar]
getBars bounds prices = map (createBar bounds prices) [0 .. length prices - 1]

createBar :: Box -> [Price] -> Int -> Bar
createBar bounds prices index = Bar
  { translateX = translateX
  , translateY = translateY
  , barWidth = barWidth
  , barHeight = barHeight
  }
  where
    translateX = boxWidth bounds / 2 - barWidth / 2
        - barWidth * realToFrac index
    translateY = -(boxHeight bounds / 2) + barHeight / 2
        + lowerHeight

    numPrices = length prices
    barWidth = boxWidth bounds / realToFrac numPrices
    maxPrice = maximum $ map high prices
    minPrice = minimum $ map low prices
    totalRange = maxPrice - minPrice

    price = prices !! index
    priceRange = high price - low price
    heightPercentage = priceRange / totalRange
    barHeight = boxHeight bounds * realToFrac heightPercentage

    lowerRange = low price - minPrice
    lowerHeightPercentage = lowerRange / totalRange
    lowerHeight = boxHeight bounds * realToFrac lowerHeightPercentage

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

