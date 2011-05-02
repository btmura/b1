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
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

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
  maybe (return stuff)
      (either (\priceData -> do
          let finalAlpha = min alpha $ (fst . current) alphaAnimation
          mapM_ (renderBar finalAlpha) $ getBars bounds (prices priceData)
          return stuff
            { volumeAlphaAnimation = next alphaAnimation
            , volumeIsDirty = isDirty || (snd . current) alphaAnimation
            }
          )
          (\_ -> return stuff))
      maybePricesData

data Bar = Bar
  { translateX :: GLfloat
  , translateY :: GLfloat
  , barWidth :: GLfloat
  , barHeight :: GLfloat
  , barColor :: GLfloat -> Color4 GLfloat
  }

renderBar :: GLfloat -> Bar -> IO ()
renderBar alpha bar =
  preservingMatrix $ do
    color $ barColor bar alpha
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
  , barWidth = barWidth - 1
  , barHeight = barHeight
  , barColor = barColor
  }
  where
    rightPadding = 0
    numPrices = length prices
    barWidth = (boxWidth bounds - rightPadding) / realToFrac numPrices

    translateX = boxWidth bounds / 2
        - rightPadding
        - barWidth * realToFrac index
        - barWidth / 2

    topPadding = 10
    availableHeight = boxHeight bounds - topPadding

    price = prices !! index
    maxVolume = maximum $ map volume prices
    volumePercentage = realToFrac (volume price) / realToFrac maxVolume
    barHeight = availableHeight * realToFrac volumePercentage

    translateY = -(boxHeight bounds / 2) + barHeight / 2

    barColor = if getPriceChange prices index > 0 then green else red

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

