module B1.Program.Chart.StochasticLines
  ( StochasticLinesInput(..)
  , StochasticLinesOutput(..)
  , StochasticLinesState
  , StochasticTimeSpec(..)
  , StochasticLineSpec(..)
  , drawStochasticLines
  , newStochasticLinesState
  , cleanStochasticLinesState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.List
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.OpenGL.Vbo
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data StochasticLinesInput = StochasticLinesInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  , inputState :: StochasticLinesState
  }

data StochasticLinesOutput = StochasticLinesOutput
  { outputState :: StochasticLinesState
  , isDirty :: Dirty
  }

data StochasticLinesState = StochasticLinesState
  { lineSpecs :: [StochasticLineSpec]
  , maybeVbo :: Maybe Vbo
  }

data StochasticTimeSpec = Daily | Weekly

data StochasticLineSpec = StochasticLineSpec
  { timeSpec :: StochasticTimeSpec
  , lineColorFunction :: GLfloat -> Color4 GLfloat
  , stochasticFunction :: Stochastic -> Float
  }

newStochasticLinesState :: [StochasticLineSpec] -> StochasticLinesState
newStochasticLinesState lineSpecs =
  StochasticLinesState
    { lineSpecs = lineSpecs
    , maybeVbo = Nothing
    }

cleanStochasticLinesState :: StochasticLinesState -> IO StochasticLinesState
cleanStochasticLinesState state@StochasticLinesState { maybeVbo = maybeVbo } =
  case maybeVbo of
    Just vbo -> do
      deleteVbo vbo
      return state { maybeVbo = Nothing }
    _ -> return state

drawStochasticLines :: Resources -> StochasticLinesInput
    -> IO StochasticLinesOutput
drawStochasticLines resources
    input@StochasticLinesInput
      { stockData = stockData
      , inputState = state
      } = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of 
    Just priceDataOrError ->
      either (renderPriceData input)
          (renderError state)
          priceDataOrError
    _ -> renderNothing state

renderPriceData :: StochasticLinesInput -> StockPriceData
    -> IO StochasticLinesOutput
renderPriceData
    input@StochasticLinesInput
      { bounds = bounds
      , inputState = state@StochasticLinesState
        { lineSpecs = lineSpecs
        , maybeVbo = maybeVbo
        }
      }
    priceData  = do

  vbo <- maybe (createStochasticLinesVbo lineSpecs priceData) return maybeVbo

  preservingMatrix $ do
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1
    renderVbo vbo

  return StochasticLinesOutput
    { outputState = state { maybeVbo = Just vbo }
    , isDirty = False
    }

createStochasticLinesVbo :: [StochasticLineSpec] -> StockPriceData -> IO Vbo
createStochasticLinesVbo lineSpecs priceData = do
  bufferObject <- createBufferObject vertices
  return $ VertexVbo bufferObject Lines numElements
  where
    vertices = getStochasticLines lineSpecs priceData
    numElements = length vertices `div` 2

getStochasticLines :: [StochasticLineSpec] -> StockPriceData -> [GLfloat]
getStochasticLines lineSpecs priceData =
  concat $ map (createLine priceData) lineSpecs

createLine :: StockPriceData -> StochasticLineSpec -> [GLfloat]
createLine priceData lineSpec =
  concat $ map (createLineSegment valueGroups) [0 .. length valueGroups - 1]
  where
    timeFunction = case timeSpec lineSpec of
        Daily -> stochastics
        _ -> weeklyStochastics
    values = map (stochasticFunction lineSpec) $ timeFunction priceData
    valueGroups = groupElements 2 values

createLineSegment :: [[Float]] -> Int -> [GLfloat]
createLineSegment valueGroups index =
  [ leftX, leftY
  , rightX, rightY
  ]
  where
    totalWidth = 2
    segmentWidth = realToFrac totalWidth / realToFrac (length valueGroups)
    rightX = totalWidth / 2 - realToFrac index * segmentWidth
    leftX = rightX - segmentWidth

    totalHeight = 2
    (rightValue:leftValue:_) = valueGroups !! index
    rightY = -(totalHeight / 2) + realToFrac rightValue * totalHeight
    leftY = -(totalHeight / 2) + realToFrac leftValue * totalHeight

renderError :: StochasticLinesState -> String -> IO StochasticLinesOutput
renderError state errorMessage =
  return StochasticLinesOutput
    { outputState = state
    , isDirty = False
    }

renderNothing :: StochasticLinesState -> IO StochasticLinesOutput
renderNothing state =
  return StochasticLinesOutput
    { outputState = state
    , isDirty = False
    }

