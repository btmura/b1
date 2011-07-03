module B1.Program.Chart.GraphUtils
  ( colorLineStripPoint
  , lineStripPoint
  , getPriceRange
  , getXValues
  , getY
  , heightPercentage
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Program.Chart.Colors

colorLineStripPoint :: Box -> Color3 GLfloat -> [GLfloat] -> Int -> Int
    -> [GLfloat]
colorLineStripPoint bounds color heightPercentages size index =
  let point = lineStripPoint bounds heightPercentages size index
      colorList = color3ToList color
  in if null point
       then []
       else point ++ colorList

lineStripPoint :: Box -> [GLfloat] -> Int -> Int -> [GLfloat]
lineStripPoint bounds heightPercentages size index
  | numSegments == 0 = []
  | null heightPercentages = []
  | index >= size = []
  | index >= length heightPercentages = []
  | otherwise = [x, y]
  where
    numSegments = if size > 1 then size - 1 else 0
    (totalWidth, totalHeight) = boxSize bounds
    segmentWidth = totalWidth / realToFrac numSegments
    x = boxRight bounds - realToFrac index * segmentWidth
    percentage = heightPercentages !! index
    y = boxBottom bounds + realToFrac percentage * totalHeight

heightPercentage :: (Float, Float) -> Float -> Float
heightPercentage (minimum, maximum) value = percentage
  where
    difference = value - minimum
    totalRange = maximum - minimum
    percentage = difference / totalRange

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

getPriceRange :: StockPriceData -> (Float, Float)
getPriceRange priceData
  | null allPrices = (0, 0)
  | otherwise = (adjustedMinPrice, adjustedMaxPrice)
  where
    takeElements = take $ numDailyElements priceData
    allPrices = concat $ map takeElements
        [ map high $ prices priceData
        , map low $ prices priceData
        , movingAverage25 priceData
        , movingAverage50 priceData
        , movingAverage200 priceData
        ] 
    minPrice = minimum allPrices
    maxPrice = maximum allPrices
    extra = (maxPrice - minPrice) * 0.05
    adjustedMinPrice = minPrice - extra
    adjustedMaxPrice = maxPrice + extra

