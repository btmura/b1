module B1.Program.Chart.PriceGraph
  ( PriceGraphInput(..)
  , PriceGraphOutput(..)
  , PriceGraphState
  , drawPriceGraph
  , newPriceGraphState
  , cleanPriceGraphState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Range
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.OpenGL.Vbo
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FragmentShader
import B1.Program.Chart.Resources

data PriceGraphInput = PriceGraphInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  , inputState :: PriceGraphState
  }

data PriceGraphOutput = PriceGraphOutput
  { outputState :: PriceGraphState
  , isDirty :: Dirty
  }

data PriceGraphState = PriceGraphState
  { maybeVbo :: Maybe Vbo
  , alphaAnimation :: Animation (GLfloat, Dirty)
  , dataStatus :: DataStatus
  }

data DataStatus = Loading | Received

newPriceGraphState :: PriceGraphState
newPriceGraphState =
  PriceGraphState
    { maybeVbo = Nothing
    , alphaAnimation = animateOnce $ linearRange 0 0 1
    , dataStatus = Loading
    }

cleanPriceGraphState :: PriceGraphState -> IO PriceGraphState
cleanPriceGraphState state@PriceGraphState { maybeVbo = maybeVbo } =
  case maybeVbo of
    Just vbo -> do
      deleteVbo vbo
      return state { maybeVbo = Nothing }
    _ -> return state

drawPriceGraph :: Resources -> PriceGraphInput -> IO PriceGraphOutput
drawPriceGraph resources
    input@PriceGraphInput
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

renderPriceData :: Resources -> PriceGraphInput -> StockPriceData
    -> IO PriceGraphOutput
renderPriceData
    Resources { program = program }
    input@PriceGraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@PriceGraphState
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

  return PriceGraphOutput
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
  bufferObject <- createBufferObject vertices
  return $ VertexVbo bufferObject Lines numElements
  where
    vertices = getGraphLineVertices $ prices priceData
    numElements = length vertices `div` 2

getGraphLineVertices :: [Price] -> [GLfloat]
getGraphLineVertices prices =
  concat $ map (createLine prices) [0 .. length prices - 1]

createLine :: [Price] -> Int -> [GLfloat]
createLine prices index =
  [ centerX, lowY
  , centerX, highY

  , leftX, openY
  , centerX, openY
  
  , centerX, closeY
  , rightX, closeY
  ]
  where
    totalWidth = 2
    barWidth = realToFrac totalWidth / realToFrac (length prices)
    halfBarWidth = barWidth / 2

    centerX = totalWidth / 2 - halfBarWidth - realToFrac index * barWidth
    leftX = centerX - halfBarWidth
    rightX = centerX + halfBarWidth

    price = prices !! index
    lowY = getY prices $ low price
    highY = getY prices $ high price
    openY = getY prices $ open price
    closeY = getY prices $ close price

getY :: [Price] -> Float -> GLfloat
getY prices value = y
  where
    maxPrice = maximum $ map high prices
    minPrice = minimum $ map low prices
    totalRange = maxPrice - minPrice

    totalHeight = 2
    range = value - minPrice
    heightPercentage = range / totalRange
    height = totalHeight * realToFrac heightPercentage

    y = -(totalHeight / 2) + height

renderError :: PriceGraphState -> String -> IO PriceGraphOutput
renderError state errorMessage = 
  return PriceGraphOutput
    { outputState = state
    , isDirty = False
    }

renderNothing :: PriceGraphState -> IO PriceGraphOutput
renderNothing state =
  return PriceGraphOutput
    { outputState = state
    , isDirty = False
    }


