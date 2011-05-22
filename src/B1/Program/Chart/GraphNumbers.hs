module B1.Program.Chart.GraphNumbers
  ( GraphNumbersInput(..)
  , drawGraphNumbers
  , getPreferredWidth
  ) where

import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.FTGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Resources

data GraphNumbersInput = GraphNumbersInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  }

format = "%0.2f"
padding = 5

getTextSpec :: Resources -> String -> TextSpec
getTextSpec resources = TextSpec (monoFont resources) 12

getPreferredWidth :: Resources -> StockData -> IO GLfloat
getPreferredWidth resources stockData = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceData ->
      either (getPriceDataWidth resources) (\_ -> return 0) priceData
    _ -> return 0

getPriceDataWidth :: Resources -> StockPriceData -> IO GLfloat
getPriceDataWidth resources priceData = do
  textBox <- measureText textSpec
  return $ padding * 2 + boxWidth textBox
  where
    (lowest, highest) = getPriceRange priceData
    textSpec = getTextSpec resources $ printf format lowest

getPriceRange :: StockPriceData -> (Float, Float)
getPriceRange priceData = (lowest, highest)
  where
    trim = take $ numDailyElements priceData
    lowest = minimum $ concat
      [ map low $ trim $ prices priceData
      , trim $ movingAverage25 priceData
      , trim $ movingAverage50 priceData
      , trim $ movingAverage200 priceData
      ]
    highest = maximum $ concat
      [ map high $ trim $ prices priceData
      , trim $ movingAverage25 priceData
      , trim $ movingAverage50 priceData
      , trim $ movingAverage200 priceData
      ]

drawGraphNumbers :: Resources -> GraphNumbersInput -> IO ()
drawGraphNumbers resources
    input@GraphNumbersInput
      { bounds = bounds
      , alpha = alpha
      , stockData = stockData
      } = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceData ->
      either (renderGraphNumbers resources bounds alpha)
          (\_ -> return ()) priceData
    _ -> return () 

renderGraphNumbers :: Resources -> Box -> GLfloat -> StockPriceData -> IO ()
renderGraphNumbers resources bounds alpha priceData =
  preservingMatrix $ do
    color $ green alpha
    translate $ vector3 translateLeft translateTop 0

    textBox <- measureText $ textSpecFunc highest
    let textHeight = boxHeight textBox
        totalHeight = boxHeight bounds

        spacingHeight = textHeight * 3

        numLabels = floor $ totalHeight / spacingHeight
        labelPrices = map getLabelPrice [0 .. numLabels - 1]

        getLabelPrice :: Int -> Float
        getLabelPrice index = price
          where
            relativeHeight = totalHeight
                - (spacingHeight * realToFrac (index + 1))
                + textHeight / 2
            heightPercentage = relativeHeight / totalHeight
            priceRange = highest - lowest
            price = lowest + realToFrac heightPercentage * priceRange

        translateDown = -spacingHeight

    mapM_ (\price -> do
        translate $ vector3 0 translateDown 0
        renderFunc price
        ) labelPrices
  where
    textSpec = getTextSpec resources
    translateLeft = -(boxWidth bounds / 2) + padding
    translateTop = boxHeight bounds / 2

    textSpecFunc :: Float -> TextSpec
    textSpecFunc value = textSpec $ printf format value

    renderFunc :: Float -> IO ()
    renderFunc = renderText . textSpecFunc

    (lowest, highest) = getPriceRange priceData

