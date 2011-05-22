module B1.Program.Chart.Graph
  ( GraphInput(..)
  , GraphOutput(..)
  , GraphState
  , drawGraph
  , newGraphState
  , cleanGraphState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.List
import B1.Data.Price
import B1.Data.Range
import B1.Data.Technicals.MovingAverage
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FragmentShader
import B1.Program.Chart.Resources
import B1.Program.Chart.StochasticColors
import B1.Program.Chart.Vbo

data GraphInput = GraphInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  , inputState :: GraphState
  }

data GraphOutput = GraphOutput
  { outputState :: GraphState
  , isDirty :: Dirty
  }

data GraphState = GraphState
  { maybeVbo :: Maybe Vbo
  , alphaAnimation :: Animation (GLfloat, Dirty)
  , dataStatus :: DataStatus
  }

data DataStatus = Loading | Received

newGraphState :: GraphState
newGraphState =
  GraphState
    { maybeVbo = Nothing
    , alphaAnimation = animateOnce $ linearRange 0 0 1
    , dataStatus = Loading
    }

cleanGraphState :: GraphState -> IO GraphState
cleanGraphState state@GraphState { maybeVbo = maybeVbo } =
  case maybeVbo of
    Just vbo -> do
      deleteVbo vbo
      return state { maybeVbo = Nothing }
    _ -> return state

drawGraph :: Resources -> GraphInput -> IO GraphOutput
drawGraph resources
    input@GraphInput
      { stockData = stockData
      , inputState = state
      } = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceDataOrError ->
      either (renderPriceData resources input)
          (renderError state)
          priceDataOrError
    _ -> renderNothing state

renderPriceData :: Resources -> GraphInput -> StockPriceData
    -> IO GraphOutput
renderPriceData
    Resources { program = program }
    input@GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@GraphState
        { maybeVbo = maybeVbo
        , alphaAnimation = alphaAnimation
        , dataStatus = dataStatus
        }
      }
    priceData = do

  vbo <- maybe (createGraphVbo priceData) return maybeVbo

  preservingMatrix $ do
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1 
    currentProgram $= Just program
    setAlpha program finalAlpha
    renderVbo vbo
    currentProgram $= Nothing

  return GraphOutput
    { outputState = state
      { maybeVbo = Just vbo
      , alphaAnimation = nextAlphaAnimation
      , dataStatus = Received
      }
    , isDirty = nextIsDirty
    }
  where
    currentAlphaAnimation = case dataStatus of
        Loading -> animateOnce $ linearRange 0 1 30
        Received -> alphaAnimation
    finalAlpha = (min alpha . fst . current) currentAlphaAnimation
    nextAlphaAnimation = next currentAlphaAnimation
    nextIsDirty = (snd . current) nextAlphaAnimation

createGraphVbo :: StockPriceData -> IO Vbo
createGraphVbo priceData = do
  putStrLn $ "Price graph size: " ++ show size
  createVbo Lines size elements
  where
    size = getTotalSize priceData
    elements = getGraphLineVertices priceData

getTotalSize :: StockPriceData -> Int
getTotalSize priceData = sum
    [ getCandlesticksSize priceData
    , getLineSize $ movingAverage25 priceData
    , getLineSize $ movingAverage50 priceData
    , getLineSize $ movingAverage200 priceData
    ]

getCandlesticksSize :: StockPriceData -> Int
getCandlesticksSize priceData = size
  where
    numElements = numDailyElements priceData
    numLines = 3 * numElements
    size = numLines * (2 * (2 + 3))

getLineSize :: [a] -> Int
getLineSize list = size
  where
    numElements = length list
    numLines = if numElements <= 1 then 0 else numElements - 1
    size = numLines * (2 * (2 + 3))

getGraphLineVertices :: StockPriceData -> [GLfloat]
getGraphLineVertices priceData =
  concat $ priceLines
      ++ movingAverage25Lines
      ++ movingAverage50Lines
      ++ movingAverage200Lines
  where
    priceRange = getPriceRange priceData
    colors = getStochasticColors $ stochastics priceData
    indices = [0 .. numDailyElements priceData - 1]
    numElements = length indices

    priceLines = map (createCandlestick priceRange
        (prices priceData) colors numElements) indices
    movingAverage25Lines = map (createMovingAverageLine priceRange
        (movingAverage25 priceData) purple3 numElements) indices
    movingAverage50Lines = map (createMovingAverageLine priceRange
        (movingAverage50 priceData) yellow3 numElements) indices
    movingAverage200Lines = map (createMovingAverageLine priceRange
        (movingAverage200 priceData) white3 numElements) indices

getPriceRange :: StockPriceData -> (Float, Float)
getPriceRange priceData = (minimum allPrices, maximum allPrices)
  where
    numElements = numDailyElements priceData
    takeElements = take numElements
    highPrices = map high $ takeElements $ prices priceData
    lowPrices = map low $ takeElements $ prices priceData
    allPrices = concat
        [ lowPrices
        , highPrices
        , takeElements $ movingAverage25 priceData
        , takeElements $ movingAverage50 priceData
        , takeElements $ movingAverage200 priceData
        ] 

createCandlestick :: (Float, Float) -> [Price] -> [Color3 GLfloat]
    -> Int -> Int -> [GLfloat]
createCandlestick priceRange prices colors numElements index =
  [centerX, lowY] ++ colorList
      ++ [centerX, highY] ++ colorList
      ++ [leftX, openY] ++ colorList
      ++ [centerX, openY] ++ colorList
      ++ [centerX, closeY] ++ colorList
      ++ [rightX, closeY] ++ colorList
  where
    colorList = color3ToList $
        if index < length colors
          then colors !! index
          else
            if getPriceChange prices index >= 0
              then green3
              else red3

    (leftX, centerX, rightX) = getXValues numElements index

    price = prices !! index
    lowY = getY priceRange $ low price
    highY = getY priceRange $ high price
    openY = getY priceRange $ open price
    closeY = getY priceRange $ close price

createMovingAverageLine :: (Float, Float) -> [MovingAverage] -> Color3 GLfloat
    -> Int -> Int -> [GLfloat]
createMovingAverageLine priceRange movingAverages color numElements index
  | index >= length valueGroups = []
  | otherwise = [leftX, leftY] ++ colorList
      ++ [rightX, rightY] ++ colorList
  where
    colorList = color3ToList color
    (leftX, _, rightX) = getXValues numElements index
    valueGroups = groupElements 2 movingAverages
    (rightValue:leftValue:_) = valueGroups !! index
    leftY = getY priceRange leftValue
    rightY = getY priceRange rightValue

getXValues :: Int -> Int -> (GLfloat, GLfloat, GLfloat)
getXValues numElements index = (leftX, centerX, rightX)
  where
    totalWidth = 2
    barWidth = realToFrac totalWidth / realToFrac numElements
    halfBarWidth = barWidth / 2
    centerX = totalWidth / 2 - halfBarWidth - realToFrac index * barWidth
    leftX = centerX - halfBarWidth
    rightX = centerX + halfBarWidth

getY :: (Float, Float) -> Float -> GLfloat
getY (minPrice, maxPrice) value = y
  where
    range = value - minPrice
    totalRange = maxPrice - minPrice

    totalHeight = 2
    heightPercentage = range / totalRange
    height = totalHeight * realToFrac heightPercentage

    y = -(totalHeight / 2) + height

renderError :: GraphState -> String -> IO GraphOutput
renderError state errorMessage = 
  return GraphOutput
    { outputState = state
    , isDirty = False
    }

renderNothing :: GraphState -> IO GraphOutput
renderNothing state =
  return GraphOutput
    { outputState = state
    , isDirty = False
    }


