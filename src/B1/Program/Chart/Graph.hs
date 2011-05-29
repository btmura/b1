module B1.Program.Chart.Graph
  ( GraphInput(..)
  , GraphOutput(..)
  , GraphState
  , GraphBoundSet(..)
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

import qualified B1.Program.Chart.StochasticLines as S
import qualified B1.Program.Chart.VolumeBars as V

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
  , boundSet :: GraphBoundSet
  }

data DataStatus = Loading | Received

data GraphBoundSet = GraphBoundSet
  { graphBounds :: Maybe Box
  , volumeBounds :: Maybe Box
  , stochasticsBounds :: Maybe Box
  , weeklyStochasticsBounds :: Maybe Box
  }

newGraphState :: GraphBoundSet -> GraphState
newGraphState boundSet =
  GraphState
    { maybeVbo = Nothing
    , alphaAnimation = animateOnce $ linearRange 0 0 1
    , dataStatus = Loading
    , boundSet = boundSet
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
        , boundSet = boundSet
        }
      }
    priceData = do

  vbo <- maybe (createGraphVbo boundSet priceData) return maybeVbo

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

createGraphVbo :: GraphBoundSet -> StockPriceData -> IO Vbo
createGraphVbo boundSet priceData = 
  createVbo $ concat
    [ getVboSpecList graphBounds $
        getGraphVboSpec priceData
    , getVboSpecList volumeBounds $
        V.getVolumeBarsVboSpec priceData
    , getVboSpecList stochasticsBounds $
        S.getStochasticLinesVboSpec priceData dailySpecs 
    , getVboSpecList weeklyStochasticsBounds $
        S.getStochasticLinesVboSpec priceData weeklySpecs
    ]
  where
    getVboSpecList :: (GraphBoundSet -> Maybe Box) -> (Box -> VboSpec)
        -> [VboSpec]
    getVboSpecList boundFunc vboFunc = case boundFunc boundSet of 
      Just bounds -> [vboFunc bounds]
      _ -> []

    dailySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Daily
        , S.lineColor = red3
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Daily
        , S.lineColor = yellow3
        , S.stochasticFunction = d
        }
      ]

    weeklySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Weekly
        , S.lineColor = red3
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Weekly
        , S.lineColor = purple3
        , S.stochasticFunction = d
        }
      ]

getGraphVboSpec :: StockPriceData -> Box -> VboSpec
getGraphVboSpec priceData graphBounds =
  VboSpec Lines graphSize graphElements
  where
    graphSize = getTotalSize priceData
    graphElements = getGraphLineVertices priceData graphBounds

getTotalSize :: StockPriceData -> Int
getTotalSize priceData = sum
    [ getCandlesticksSize priceData
    , getLineSize $ trim $ movingAverage25 priceData
    , getLineSize $ trim $ movingAverage50 priceData
    , getLineSize $ trim $ movingAverage200 priceData
    ]
  where
    trim = take $ numDailyElements priceData

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

getGraphLineVertices :: StockPriceData -> Box -> [GLfloat]
getGraphLineVertices priceData bounds =
  concat $ priceLines
      ++ movingAverage25Lines
      ++ movingAverage50Lines
      ++ movingAverage200Lines
  where
    priceRange = getPriceRange priceData
    colors = getStochasticColors $ stochastics priceData
    numElements = numDailyElements priceData
    trim = take numElements
    indices = [0 .. numElements - 1]

    priceLines = map (createCandlestick bounds priceRange
        (prices priceData) colors numElements) indices
    movingAverage25Lines = map (createMovingAverageLine bounds priceRange
        (trim (movingAverage25 priceData)) purple3 numElements) indices
    movingAverage50Lines = map (createMovingAverageLine bounds priceRange
        (trim (movingAverage50 priceData)) yellow3 numElements) indices
    movingAverage200Lines = map (createMovingAverageLine bounds priceRange
        (trim (movingAverage200 priceData)) white3 numElements) indices

getPriceRange :: StockPriceData -> (Float, Float)
getPriceRange priceData = (minimum allPrices, maximum allPrices)
  where
    takeElements = take $ numDailyElements priceData
    allPrices = concat $ map takeElements
        [ map high $ prices priceData
        , map low $ prices priceData
        , movingAverage25 priceData
        , movingAverage50 priceData
        , movingAverage200 priceData
        ] 

createCandlestick :: Box -> (Float, Float) -> [Price] -> [Color3 GLfloat]
    -> Int -> Int -> [GLfloat]
createCandlestick bounds priceRange prices colors numElements index =
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

    (leftX, centerX, rightX) = getXValues bounds numElements index

    price = prices !! index
    lowY = getY bounds priceRange $ low price
    highY = getY bounds priceRange $ high price
    openY = getY bounds priceRange $ open price
    closeY = getY bounds priceRange $ close price

createMovingAverageLine :: Box -> (Float, Float) -> [MovingAverage]
    -> Color3 GLfloat -> Int -> Int -> [GLfloat]
createMovingAverageLine bounds priceRange movingAverages color numElements index
  | index >= length valueGroups = []
  | otherwise = [leftX, leftY] ++ colorList
      ++ [rightX, rightY] ++ colorList
  where
    colorList = color3ToList color
    (leftX, _, rightX) = getXValues bounds numElements index
    valueGroups = groupElements 2 movingAverages
    (rightValue:leftValue:_) = valueGroups !! index
    leftY = getY bounds priceRange leftValue
    rightY = getY bounds priceRange rightValue

getXValues :: Box -> Int -> Int -> (GLfloat, GLfloat, GLfloat)
getXValues bounds numElements index = (leftX, centerX, rightX)
  where
    totalWidth = boxWidth bounds
    barWidth = realToFrac totalWidth / realToFrac numElements
    halfBarWidth = barWidth / 2
    centerX = boxRight bounds - halfBarWidth - realToFrac index * barWidth
    leftX = centerX - halfBarWidth
    rightX = centerX + halfBarWidth

getY :: Box -> (Float, Float) -> Float -> GLfloat
getY bounds (minPrice, maxPrice) value = y
  where
    range = value - minPrice
    totalRange = maxPrice - minPrice

    totalHeight = boxHeight bounds
    heightPercentage = range / totalRange
    height = totalHeight * realToFrac heightPercentage

    y = boxBottom bounds + height

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


