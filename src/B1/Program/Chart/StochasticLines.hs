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
import B1.Data.Range
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FragmentShader
import B1.Program.Chart.Resources
import B1.Program.Chart.Vbo

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
  , alphaAnimation :: Animation (GLfloat, Dirty)
  , dataStatus :: DataStatus
  }

data StochasticTimeSpec = Daily | Weekly

data StochasticLineSpec = StochasticLineSpec
  { timeSpec :: StochasticTimeSpec
  , lineColor :: Color3 GLfloat
  , stochasticFunction :: Stochastic -> Float
  }

data DataStatus = Loading | Received

newStochasticLinesState :: [StochasticLineSpec] -> StochasticLinesState
newStochasticLinesState lineSpecs =
  StochasticLinesState
    { lineSpecs = lineSpecs
    , maybeVbo = Nothing
    , alphaAnimation = animateOnce $ linearRange 0 0 1
    , dataStatus = Loading
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
      either (renderPriceData resources input)
          (renderError state)
          priceDataOrError
    _ -> renderNothing state

renderPriceData :: Resources -> StochasticLinesInput -> StockPriceData
    -> IO StochasticLinesOutput
renderPriceData
    Resources { program = program }
    input@StochasticLinesInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@StochasticLinesState
        { lineSpecs = lineSpecs
        , maybeVbo = maybeVbo
        , alphaAnimation = alphaAnimation
        , dataStatus = dataStatus
        }
      }
    priceData  = do

  vbo <- maybe (createStochasticLinesVbo lineSpecs priceData) return maybeVbo

  preservingMatrix $ do
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1
    currentProgram $= Just program
    setAlpha program finalAlpha
    renderVbo vbo
    currentProgram $= Nothing

  return StochasticLinesOutput
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

createStochasticLinesVbo :: [StochasticLineSpec] -> StockPriceData -> IO Vbo
createStochasticLinesVbo lineSpecs priceData = do
  putStrLn $ "Stochastic lines size: " ++ show size
  createVbo Lines size elements
  where
    size = getTotalSize lineSpecs priceData
    elements = getStochasticLines lineSpecs priceData

getTotalSize :: [StochasticLineSpec] -> StockPriceData -> Int
getTotalSize lineSpecs priceData = 
  sum $ map (getSize priceData) lineSpecs

getSize :: StockPriceData -> StochasticLineSpec -> Int
getSize priceData lineSpec = size
  where
    numElementsFunction = case timeSpec lineSpec of
        Daily -> numDailyElements
        _ -> numWeeklyElements
    numElements = numElementsFunction priceData
    numLines = if numElements <= 1 then 0 else numElements - 1
    size = numLines * (2 * (2 + 3))

getStochasticLines :: [StochasticLineSpec] -> StockPriceData -> [GLfloat]
getStochasticLines lineSpecs priceData =
  concat $ map (createLine priceData) lineSpecs

createLine :: StockPriceData -> StochasticLineSpec -> [GLfloat]
createLine priceData lineSpec =
  concat $ map (createLineSegment color valueGroups numGroups) indices
  where
    color = lineColor lineSpec
    (dataFunction, numElements) = case timeSpec lineSpec of
        Daily -> (stochastics, numDailyElements)
        _ -> (weeklyStochastics, numWeeklyElements)
    valueGroups = (groupElements 2
        . map (stochasticFunction lineSpec)
        . take (numElements priceData)
        . dataFunction
        ) priceData
    numGroups = length valueGroups
    indices = [0 .. length valueGroups - 1]

createLineSegment :: Color3 GLfloat -> [[Float]] -> Int -> Int -> [GLfloat]
createLineSegment color valueGroups numElements index
  | index >= length valueGroups = []
  | otherwise = [leftX, leftY] ++ colorList ++ [rightX, rightY] ++ colorList
  where
    colorList = color3ToList color
    totalWidth = 2
    segmentWidth = realToFrac totalWidth / realToFrac numElements
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

