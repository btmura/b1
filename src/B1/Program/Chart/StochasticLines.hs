module B1.Program.Chart.StochasticLines
  ( StochasticLinesInput(..)
  , StochasticLinesOutput(..)
  , StochasticLinesState
  , drawStochasticLines
  , newStochasticLinesState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.List
import B1.Data.Technicals.Stochastic
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.StockData

data StochasticLinesInput = StochasticLinesInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: StochasticLinesState
  }

data StochasticLinesOutput = StochasticLinesOutput
  { outputState :: StochasticLinesState
  , isDirty :: Dirty
  }

data StochasticLinesState = StochasticLinesState
  { stockData :: StockData
  }

newStochasticLinesState :: StockData -> StochasticLinesState
newStochasticLinesState stockData =
  StochasticLinesState
    { stockData = stockData
    }

drawStochasticLines :: Resources -> StochasticLinesInput
    -> IO StochasticLinesOutput
drawStochasticLines resources input = 
  convertInputToStuff input
      >>= renderStuff
      >>= convertStuffToOutput

data StochasticStuff = StochasticStuff
  { stoBounds :: Box
  , stoAlpha :: GLfloat
  , stoStockData :: StockData
  , stoIsDirty :: Dirty
  }

convertInputToStuff :: StochasticLinesInput -> IO StochasticStuff
convertInputToStuff
    StochasticLinesInput
      { bounds = bounds
      , alpha = alpha
      , inputState = StochasticLinesState
        { stockData = stockData
        }
      } =
  return StochasticStuff
    { stoBounds = bounds
    , stoAlpha = alpha
    , stoStockData = stockData
    , stoIsDirty = False
    }

renderStuff :: StochasticStuff -> IO StochasticStuff
renderStuff stuff = do
  maybePriceData <- getStockPriceData $ stoStockData stuff
  maybe (return stuff { stoIsDirty = True })
      (\priceData -> either
          (renderLines stuff)
          (renderError stuff)
          (dailyStochasticsOrError priceData))
      maybePriceData

renderLines :: StochasticStuff -> [Stochastic] -> IO StochasticStuff
renderLines
    stuff@StochasticStuff
      { stoBounds = bounds
      , stoAlpha = alpha
      }
    stochastics =
  preservingMatrix $ do
    lineWidth $= 2
    renderPrimitive Lines $ mapM_ (renderLineSegment alpha) lines
    return stuff { stoIsDirty = False }
  where 
    lines = concat
        [ getLineSegments bounds red $ map k stochastics
        , getLineSegments bounds yellow $ map d stochastics
        ]

data LineSegment = LineSegment
  { leftPoint :: Point
  , rightPoint :: Point
  , lineColor :: GLfloat -> Color4 GLfloat
  }

renderLineSegment :: GLfloat -> LineSegment -> IO ()
renderLineSegment alpha lineSegment = do
  color $ lineColor lineSegment alpha
  vertex $ vertex2 leftX leftY
  vertex $ vertex2 rightX rightY
  where
    (leftX, leftY) = leftPoint lineSegment
    (rightX, rightY) = rightPoint lineSegment

getLineSegments :: Box -> (GLfloat -> Color4 GLfloat) -> [Float]
    -> [LineSegment]
getLineSegments bounds lineColor stochasticValues = 
  map (createLineSegment bounds lineColor stochasticGroups) groupIndices
  where
    stochasticGroups = groupElements 2 $ reverse stochasticValues
    groupIndices = [0 .. length stochasticGroups - 1]

createLineSegment :: Box -> (GLfloat -> Color4 GLfloat) -> [[Float]] -> Int
    -> LineSegment
createLineSegment bounds lineColor stochasticGroups index = LineSegment
  { leftPoint = (leftX, leftY)
  , rightPoint = (rightX, rightY)
  , lineColor = lineColor
  }
  where
    numGroups = length stochasticGroups
    lineWidth = boxWidth bounds / realToFrac numGroups
    leftX = -(boxWidth bounds / 2) + lineWidth * realToFrac index
    rightX = leftX + lineWidth

    topPadding = 5
    bottomPadding = 5
    maxHeight = realToFrac $ boxHeight bounds - topPadding - bottomPadding
    (leftValue:rightValue:_) = stochasticGroups !! index
    leftY = -(boxHeight bounds / 2) + bottomPadding
        + realToFrac leftValue * maxHeight
    rightY = -(boxHeight bounds / 2) + bottomPadding
        + realToFrac rightValue * maxHeight

renderError :: StochasticStuff -> String -> IO StochasticStuff
renderError stuff errorMessage = return stuff

convertStuffToOutput :: StochasticStuff -> IO StochasticLinesOutput
convertStuffToOutput
    StochasticStuff
      { stoStockData = stockData
      , stoIsDirty = isDirty
      } =
  return StochasticLinesOutput
    { outputState = StochasticLinesState
      { stockData = stockData
      }
    , isDirty = isDirty
    }
