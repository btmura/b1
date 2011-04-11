module B1.Program.Chart.VolumeBars
  ( VolumeBarsInput(..)
  , VolumeBarsOutput(..)
  , VolumeBarsState
  , drawVolumeBars
  , newVolumeBarsState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
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
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

newVolumeBarsState :: StockData -> VolumeBarsState
newVolumeBarsState stockData =
  VolumeBarsState
    { stockData = stockData
    , alphaAnimation = animateOnce $ linearRange 0 1 20
    }

drawVolumeBars :: Resources -> VolumeBarsInput -> IO VolumeBarsOutput
drawVolumeBars resources input =
  convertInputToStuff input
      >>= renderVolumeBars
      >>= convertStuffToOutput

data VolumeBarsStuff = VolumeBarsStuff
  { volumeBounds :: Box
  , volumeAlpha :: GLfloat
  , volumeStockData :: StockData
  , volumeAlphaAnimation :: Animation (GLfloat, Dirty)
  , volumeIsDirty :: Dirty
  }

convertInputToStuff :: VolumeBarsInput -> IO VolumeBarsStuff
convertInputToStuff
    VolumeBarsInput
      { bounds = bounds
      , alpha = alpha
      , inputState = VolumeBarsState
        { stockData = stockData
        , alphaAnimation = alphaAnimation
        }
      } = 
  return VolumeBarsStuff
    { volumeBounds = bounds
    , volumeAlpha = alpha
    , volumeStockData = stockData
    , volumeAlphaAnimation = alphaAnimation
    , volumeIsDirty = False
    }

renderVolumeBars :: VolumeBarsStuff -> IO VolumeBarsStuff
renderVolumeBars 
    stuff@VolumeBarsStuff
      { volumeBounds = bounds
      , volumeAlpha = alpha
      , volumeStockData = stockData
      , volumeAlphaAnimation = alphaAnimation
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
              finalAlpha = min alpha $ (fst . current) alphaAnimation
          preservingMatrix $ do
            translate $ vector3 (-(boxWidth bounds) / 2)
                (-(boxHeight bounds) / 2) 0
            mapM_ (\index -> do
              renderSingleBar allPrices index barWidth maxBarHeight finalAlpha
                  maxVolume
              translate $ vector3 barWidth 0 0
              ) [0 .. length allPrices - 1]
          return stuff
            { volumeAlphaAnimation = next alphaAnimation
            , volumeIsDirty = isDirty || (snd . current) alphaAnimation
            }
        _ -> return stuff
    _ -> return stuff { volumeIsDirty = True }

renderSingleBar :: [Price] -> Int -> GLfloat -> GLfloat -> GLfloat -> Int
    -> IO ()
renderSingleBar prices index barWidth maxBarHeight alpha maxVolume = do
  color $ barColor alpha
  renderPrimitive Quads $ do
    vertex $ vertex2 1 0
    vertex $ vertex2 1 barHeight
    vertex $ vertex2 barWidth barHeight
    vertex $ vertex2 barWidth 0
  where
    barColor = getBarColor prices index
    percentage = realToFrac (volume (prices !! index)) / realToFrac maxVolume
    barHeight = maxBarHeight * percentage

getBarColor :: [Price] -> Int -> (GLfloat -> Color4 GLfloat)
getBarColor prices index 
  | length prices < 2 = gray
  | index < 1 = gray
  | change >= 0 = green
  | otherwise = red
  where
    previousClose = close $ prices !! (index - 1)
    currentClose = close $ prices !! index
    change = currentClose - previousClose


convertStuffToOutput :: VolumeBarsStuff -> IO VolumeBarsOutput
convertStuffToOutput 
    VolumeBarsStuff
      { volumeStockData = stockData
      , volumeAlphaAnimation = alphaAnimation
      , volumeIsDirty = isDirty
      } =
  return VolumeBarsOutput
    { outputState = VolumeBarsState
      { stockData = stockData
      , alphaAnimation = alphaAnimation
      }
    , isDirty = isDirty
    }

