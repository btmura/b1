module B1.Program.Chart.PriceGraph
  ( PriceGraphInput(..)
  , PriceGraphOutput(..)
  , PriceGraphState
  , drawPriceGraph
  , newPriceGraphState
  ) where

import Graphics.Rendering.OpenGL

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

