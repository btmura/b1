module B1.Program.Chart.StochasticLines
  ( StochasticTimeSpec(..)
  , StochasticLineSpec(..)
  , getVboSpecs
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
import B1.Program.Chart.StochasticColors
import B1.Program.Chart.Vbo

data StochasticLineSpec = StochasticLineSpec
  { timeSpec :: StochasticTimeSpec
  , lineColor :: Color3 GLfloat
  , stochasticFunction :: Stochastic -> Float
  }

data StochasticTimeSpec = Daily | Weekly

floatsPerVertex = 2 + 3 -- x, y, and 3 for color

getVboSpecs :: StockPriceData -> [StochasticLineSpec] -> Box -> [VboSpec]
getVboSpecs priceData lineSpecs bounds =
  getBackgroundVboSpecs priceData lineSpecs bounds
      ++ getPercentageLineVboSpecs bounds
      ++ getLineVboSpecs priceData lineSpecs bounds

getBackgroundVboSpecs :: StockPriceData -> [StochasticLineSpec] -> Box
    -> [VboSpec]
getBackgroundVboSpecs priceData lineSpecs bounds =
  [VboSpec Quads size elements]
  where
    numQuads = 1
    size = numQuads * (4 * floatsPerVertex)
    elements = getBackgroundElements priceData lineSpecs bounds

getBackgroundElements :: StockPriceData -> [StochasticLineSpec] -> Box
    -> [GLfloat]
getBackgroundElements priceData lineSpecs bounds
  | null lineSpecs = []
  | null stochasticColors = []
  | otherwise = elements
  where
    dataFunction = case timeSpec (head lineSpecs) of
        Daily -> stochastics
        _ -> weeklyStochastics
    stochasticColors = getStochasticColors $ dataFunction priceData
    color = color3ToList $ head stochasticColors

    Box (left, top) (right, bottom) = bounds
    elements =
      -- Top Quad
      [ left
      , bottom
      , 0, 0, 0

      , left
      , top
      , 0, 0, 0

      , right
      , top
      , 0, 0, 0

      , right
      , bottom
      ] ++ color ++

      []

getLineVboSpecs :: StockPriceData -> [StochasticLineSpec] -> Box -> [VboSpec]
getLineVboSpecs priceData lineSpecs bounds =
  map (createLineVboSpec priceData bounds) lineSpecs

createLineVboSpec :: StockPriceData -> Box -> StochasticLineSpec -> VboSpec
createLineVboSpec priceData bounds lineSpec =
  VboSpec LineStrip size elements
  where
    size = getLineSize priceData lineSpec
    elements = createLine priceData bounds lineSpec

getLineSize :: StockPriceData -> StochasticLineSpec -> Int
getLineSize priceData lineSpec = size
  where
    numElementsFunction = case timeSpec lineSpec of
        Daily -> numDailyElements
        _ -> numWeeklyElements
    numElements = numElementsFunction priceData
    size = numElements * floatsPerVertex

createLine :: StockPriceData -> Box -> StochasticLineSpec -> [GLfloat]
createLine priceData bounds lineSpec =
  concat $ map (createLineSegment bounds color values) indices
  where
    color = lineColor lineSpec
    (dataFunction, numElements) = case timeSpec lineSpec of
        Daily -> (stochastics, numDailyElements)
        _ -> (weeklyStochastics, numWeeklyElements)
    values = map (stochasticFunction lineSpec) $
        take (numElements priceData) $
        dataFunction priceData
    indices = [0 .. length values - 1]

createLineSegment :: Box -> Color3 GLfloat -> [Float] -> Int -> [GLfloat]
createLineSegment bounds color values index
  | null values = []
  | otherwise = [x, y] ++ colorList
  where
    colorList = color3ToList color
    totalWidth = boxWidth bounds
    segmentWidth = realToFrac totalWidth / realToFrac (length values - 1)
    x = boxRight bounds - realToFrac index * segmentWidth

    value = values !! index
    totalHeight = boxHeight bounds
    y = boxBottom bounds + realToFrac value * totalHeight

getPercentageLineVboSpecs :: Box -> [VboSpec]
getPercentageLineVboSpecs bounds = [VboSpec Lines size elements]
  where
    numLines = 2
    size = numLines * (2 * floatsPerVertex)
    elements = getPercentageLineElements bounds

getPercentageLineElements :: Box -> [GLfloat]
getPercentageLineElements bounds@(Box (left, _) (right, bottom)) =
  [left, thirty] ++ colorList 
      ++ [right, thirty] ++ colorList 
      ++ [left, seventy] ++ colorList 
      ++ [right, seventy] ++ colorList 
  where
    colorList = color3ToList darkBlue3
    lineY percentage = bottom + (percentage * boxHeight bounds)
    thirty = lineY 0.3
    seventy = lineY 0.7

