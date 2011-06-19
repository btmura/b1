module B1.Program.Chart.MonthLines
  ( getVboSpecs
  ) where

import Data.Time.Calendar
import Data.Time.LocalTime
import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.GraphUtils
import B1.Program.Chart.Vbo

floatsPerVertex = 2 + 3 -- x, y, and 3 for color

getVboSpecs :: StockPriceData -> Box -> [VboSpec]
getVboSpecs priceData bounds =
  [VboSpec Lines size elements]
  where
    numMonths = getNumMonths priceData
    size = numMonths * (2 * floatsPerVertex)
    elements = getElements priceData bounds

getNumMonths :: StockPriceData -> Int
getNumMonths priceData
  | length stockPrices < 2 = 0
  | otherwise = numMonths
  where
    stockPrices = prices priceData
    numElements = numDailyElements priceData
    transitions = map (isMonthTransition stockPrices) [0 .. numElements - 1]
    numMonths = length $ filter (== True) transitions

isMonthTransition :: [Price] -> Int -> Bool
isMonthTransition prices index
  | index + 1 >= length prices = False
  | otherwise = isTransition
  where
    getMonth :: (Integer, Int, Int) -> Int
    getMonth (_, month, _) = month
  
    extractMonth :: Price -> Int 
    extractMonth = getMonth . toGregorian . localDay . startTime 

    month = extractMonth $ prices !! index
    nextMonth = extractMonth $ prices !! (index + 1)
    isTransition = month /= nextMonth

getElements :: StockPriceData -> Box -> [GLfloat]
getElements priceData bounds = concat lines
  where
    stockPrices = prices priceData
    numElements = numDailyElements priceData
    lines = map (getLineFloats bounds stockPrices numElements)
        [0 .. numElements - 1]

getLineFloats :: Box -> [Price] -> Int -> Int -> [GLfloat]
getLineFloats bounds prices numElements index
  | not isTransition = []
  | otherwise = [centerX, bottom] ++ colorList
      ++ [centerX, top] ++ colorList
  where
    isTransition = isMonthTransition prices index
    colorList = color3ToList darkBlue3
    (_, centerX, _) = getXValues bounds numElements index
    Box (_, top) (_, bottom) = bounds

