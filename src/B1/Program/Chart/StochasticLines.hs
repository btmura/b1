module B1.Program.Chart.StochasticLines
  ( StochasticTimeSpec(..)
  , StochasticLineSpec(..)
  , getStochasticLinesVboSpec 
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

data StochasticLineSpec = StochasticLineSpec
  { timeSpec :: StochasticTimeSpec
  , lineColor :: Color3 GLfloat
  , stochasticFunction :: Stochastic -> Float
  }

data StochasticTimeSpec = Daily | Weekly

data DataStatus = Loading | Received

getStochasticLinesVboSpec :: StockPriceData -> [StochasticLineSpec] -> Box
    -> VboSpec
getStochasticLinesVboSpec priceData lineSpecs bounds =
  VboSpec Lines size elements
  where
    size = getStochasticLinesSize lineSpecs priceData
    elements = getStochasticLines lineSpecs priceData bounds

getStochasticLinesSize :: [StochasticLineSpec] -> StockPriceData -> Int
getStochasticLinesSize lineSpecs priceData = 
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

getStochasticLines :: [StochasticLineSpec] -> StockPriceData -> Box -> [GLfloat]
getStochasticLines lineSpecs priceData bounds =
  concat $ map (createLine priceData bounds) lineSpecs

createLine :: StockPriceData -> Box -> StochasticLineSpec -> [GLfloat]
createLine priceData bounds lineSpec =
  concat $ map (createLineSegment bounds color valueGroups numGroups) indices
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

createLineSegment :: Box -> Color3 GLfloat -> [[Float]] -> Int -> Int
    -> [GLfloat]
createLineSegment bounds color valueGroups numElements index
  | index >= length valueGroups = []
  | otherwise = [leftX, leftY] ++ colorList ++ [rightX, rightY] ++ colorList
  where
    colorList = color3ToList color
    totalWidth = boxWidth bounds
    segmentWidth = realToFrac totalWidth / realToFrac numElements
    rightX = boxRight bounds - realToFrac index * segmentWidth
    leftX = rightX - segmentWidth

    totalHeight = boxHeight bounds
    (rightValue:leftValue:_) = valueGroups !! index
    rightY = boxBottom bounds + realToFrac rightValue * totalHeight
    leftY = boxBottom bounds + realToFrac leftValue * totalHeight

