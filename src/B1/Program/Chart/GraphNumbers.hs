module B1.Program.Chart.GraphNumbers
  ( GraphNumbersInput(..)
  , GraphNumbersType(..)
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

data GraphNumbersType = Prices | Volume

data GraphNumbersInput = GraphNumbersInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  , numbersType :: GraphNumbersType
  }

padding = 5

getFormat :: GraphNumbersType -> String
getFormat Prices = "%0.2f"
getFormat Volume = "%0.0f"

getTextSpec :: Resources -> String -> TextSpec
getTextSpec resources = TextSpec (monoFont resources) 12

getPreferredWidth :: Resources -> GraphNumbersType -> StockData -> IO GLfloat
getPreferredWidth resources numbersType stockData = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceData ->
      either (getValueWidth resources numbersType) (\_ -> return 0) priceData
    _ -> return 0

getValueWidth :: Resources -> GraphNumbersType -> StockPriceData -> IO GLfloat
getValueWidth resources numbersType priceData = do
  textBox <- measureText textSpec
  return $ padding * 2 + boxWidth textBox
  where
    (_, highest) = getValueRange numbersType priceData
    textSpec = getTextSpec resources $ printf (getFormat numbersType) highest

getValueRange :: GraphNumbersType -> StockPriceData -> (Float, Float)

getValueRange Prices priceData = (lowest, highest)
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

getValueRange Volume priceData = (lowest, highest)
  where
    trim = take $ numDailyElements priceData
    modulo value = value `mod` 100000
    volumes = map (modulo . volume) $ trim $ prices priceData
    lowest = realToFrac $ minimum volumes
    highest = realToFrac $ maximum volumes

drawGraphNumbers :: Resources -> GraphNumbersInput -> IO ()
drawGraphNumbers resources
    input@GraphNumbersInput
      { bounds = bounds
      , alpha = alpha
      , stockData = stockData
      , numbersType = numbersType
      } = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceData ->
      either (renderGraphNumbers resources bounds alpha numbersType)
          (\_ -> return ()) priceData
    _ -> return () 

renderGraphNumbers :: Resources -> Box -> GLfloat -> GraphNumbersType
    -> StockPriceData -> IO ()
renderGraphNumbers resources bounds alpha numbersType priceData =
  preservingMatrix $ do
    color $ yellow4 alpha
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
    textSpecFunc value = textSpec $ printf (getFormat numbersType) value

    renderFunc :: Float -> IO ()
    renderFunc = renderText . textSpecFunc

    (lowest, highest) = getValueRange numbersType priceData

