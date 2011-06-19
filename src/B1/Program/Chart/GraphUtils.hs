module B1.Program.Chart.GraphUtils
  ( getPriceRange
  , getXValues
  , getY
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box

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

