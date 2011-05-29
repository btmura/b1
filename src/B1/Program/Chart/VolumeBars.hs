module B1.Program.Chart.VolumeBars
  ( getVboSpecs
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Range
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FragmentShader
import B1.Program.Chart.Resources
import B1.Program.Chart.Streak
import B1.Program.Chart.Vbo

getVboSpecs :: StockPriceData -> Box -> [VboSpec]
getVboSpecs priceData bounds =
  [ getBackgroundVboSpec priceData bounds
  , getBarsVboSpec priceData bounds
  ]

getBackgroundVboSpec :: StockPriceData -> Box -> VboSpec
getBackgroundVboSpec priceData bounds = VboSpec Quads size elements
  where
    size = getBackgroundSize
    elements = getBackgroundElements priceData bounds

getBackgroundSize :: Int
getBackgroundSize = getStreakSize

getBackgroundElements :: StockPriceData -> Box -> [GLfloat]
getBackgroundElements priceData bounds
  | null stockPrices = [] 
  | otherwise = elements
  where
    stockPrices = prices priceData
    color = if getPriceChange stockPrices 0 >= 0 then darkGreen3 else darkRed3
    elements = getStreakElements bounds color

getBarsVboSpec :: StockPriceData -> Box -> VboSpec
getBarsVboSpec priceData bounds = VboSpec Quads size elements
  where
    size = getBarsSize priceData
    elements = getBarElements priceData bounds

getBarsSize :: StockPriceData -> Int
getBarsSize priceData = size
  where
    numElements = numDailyElements priceData
    size = numElements * (4 * (2 + 3))

getBarElements :: StockPriceData -> Box -> [GLfloat]
getBarElements priceData bounds =
  concat $ map (createBar bounds stockPrices numElements) indices
  where
    stockPrices = prices priceData
    numElements = numDailyElements priceData
    indices = [0 .. numElements - 1]

createBar :: Box -> [Price] -> Int -> Int -> [GLfloat]
createBar bounds prices numElements index =
  [leftX, bottomY] ++ colorList
      ++ [leftX, topY] ++ colorList
      ++ [rightX, topY] ++ colorList
      ++ [rightX, bottomY] ++ colorList
  where
    colorList = color3ToList $
        if getPriceChange prices index >= 0
          then green3
          else red3

    totalWidth = boxWidth bounds
    barWidth = realToFrac totalWidth / realToFrac numElements
    spacing = barWidth / 3
    rightX = boxRight bounds - realToFrac index * barWidth - spacing
    leftX = rightX - barWidth + spacing

    maxVolume = maximum $ map volume prices
    minVolume = minimum $ map volume prices
    totalRange = maxVolume - minVolume

    currentVolume = volume $ prices !! index
    range = currentVolume - minVolume
    heightPercentage = realToFrac range / realToFrac totalRange
    totalHeight = boxHeight bounds
    height = totalHeight * realToFrac heightPercentage
    bottomY = boxBottom bounds
    topY = bottomY + height

