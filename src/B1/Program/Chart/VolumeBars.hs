module B1.Program.Chart.VolumeBars
  ( VolumeBarsInput(..)
  , VolumeBarsOutput(..)
  , VolumeBarsState
  , drawVolumeBars
  , newVolumeBarsState
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

data VolumeBarsInput = VolumeBarsInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: VolumeBarsState
  }

data VolumeBarsOutput = VolumeBarsOutput
  { outputState :: VolumeBarsState
  , isDirty :: Dirty
  }

data VolumeBarsState = VolumeBarsState
  { stockData :: StockData
  }

newVolumeBarsState :: StockData -> VolumeBarsState
newVolumeBarsState stockData = VolumeBarsState { stockData = stockData }

drawVolumeBars :: Resources -> VolumeBarsInput -> IO VolumeBarsOutput
drawVolumeBars resources input =
  convertInputToStuff input
      >>= renderVolumeBars
      >>= convertStuffToOutput

data VolumeBarsStuff = VolumeBarsStuff
  { volumeBounds :: Box
  , volumeAlpha :: GLfloat
  , volumeStockData :: StockData
  , volumeIsDirty :: Dirty
  }

convertInputToStuff :: VolumeBarsInput -> IO VolumeBarsStuff
convertInputToStuff
    VolumeBarsInput
      { bounds = bounds
      , alpha = alpha
      , inputState = VolumeBarsState
        { stockData = stockData
        }
      } = 
  return VolumeBarsStuff
    { volumeBounds = bounds
    , volumeAlpha = alpha
    , volumeStockData = stockData
    , volumeIsDirty = False
    }

renderVolumeBars :: VolumeBarsStuff -> IO VolumeBarsStuff
renderVolumeBars 
    stuff@VolumeBarsStuff
      { volumeBounds = bounds
      , volumeAlpha = alpha
      , volumeStockData = stockData
      , volumeIsDirty = isDirty
      } = do
  maybePricesData <- getStockPriceData stockData
  case maybePricesData of
    Just pricesData -> do
      case prices pricesData of
        (Just allPrices, []) -> do
          let barWidth = boxWidth bounds / realToFrac (length allPrices)
              maxBarHeight = boxHeight bounds
              maxVolume = maximum $ map volume allPrices
          preservingMatrix $ do
            translate $ vector3 (-(boxWidth bounds) / 2)
                (-(boxHeight bounds) / 2) 0
            mapM_ (\price -> do
              renderSingleBar price barWidth maxBarHeight alpha maxVolume
              translate $ vector3 barWidth 0 0
              ) allPrices
        _ -> return ()
      return stuff { volumeIsDirty = True }
    _ ->
      return stuff { volumeIsDirty = True }

renderSingleBar :: Price -> GLfloat -> GLfloat -> GLfloat -> Int -> IO ()
renderSingleBar price barWidth maxBarHeight alpha maxVolume = do
  color $ barColor alpha
  renderPrimitive Quads $ do
    vertex $ vertex2 0 0
    vertex $ vertex2 0 barHeight
    vertex $ vertex2 barWidth barHeight
    vertex $ vertex2 barWidth 0
  where
    barColor = if close price - open price >= 0
      then green
      else red
    percentage = realToFrac (volume price) / realToFrac maxVolume
    barHeight = maxBarHeight * percentage

convertStuffToOutput :: VolumeBarsStuff -> IO VolumeBarsOutput
convertStuffToOutput 
    VolumeBarsStuff
      { volumeStockData = stockData
      , volumeIsDirty = isDirty
      } =
  return VolumeBarsOutput
    { outputState = VolumeBarsState { stockData = stockData }
    , isDirty = isDirty
    }

