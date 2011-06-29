module B1.Program.Chart.Candlesticks
  ( getVboSpecs
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Program.Chart.Colors
import B1.Program.Chart.GraphUtils
import B1.Program.Chart.StochasticColors
import B1.Program.Chart.Vbo

floatsPerVertex = 2 + 3 -- x, y, and 3 for color

getVboSpecs :: StockPriceData -> Box -> [VboSpec]
getVboSpecs priceData bounds =
  getBackgroundVboSpecs bounds
      ++ getCandlestickVboSpecs priceData bounds

getBackgroundVboSpecs :: Box -> [VboSpec]
getBackgroundVboSpecs bounds =
  [VboSpec Quads size elements]
  where
    numQuads = 1
    size = numQuads * (4 * floatsPerVertex)
    elements = getBackgroundElements bounds

getBackgroundElements :: Box -> [GLfloat]
getBackgroundElements bounds = elements
  where
    color = [0, 0, 0.15]
    Box (left, top) (right, bottom) = bounds
    elements =
      -- Top Quad
      [ left
      , bottom
      , 0, 0, 0

      , left
      , top
      ] ++ color ++

      [ right
      , top
      ] ++ color ++

      [ right
      , bottom
      , 0, 0, 0
      ] 

getCandlestickVboSpecs :: StockPriceData -> Box -> [VboSpec]
getCandlestickVboSpecs priceData bounds =
  [VboSpec Lines size elements]
  where
    size = getCandlesticksSize priceData
    elements = getCandlestickElements priceData bounds

getCandlesticksSize :: StockPriceData -> Int
getCandlesticksSize priceData = size
  where
    numElements = numDailyElements priceData
    numLines = 3 * numElements
    size = numLines * (2 * (2 + 3))

getCandlestickElements :: StockPriceData -> Box -> [GLfloat]
getCandlestickElements priceData bounds = concat priceLines
  where
    priceRange = getPriceRange priceData
    colors = getStochasticColors $ stochastics priceData
    numElements = numDailyElements priceData
    trim = take numElements
    indices = [0 .. numElements - 1]

    priceLines = map (createCandlestick bounds priceRange
        (prices priceData) colors numElements) indices

createCandlestick :: Box -> (Float, Float) -> [Price] -> [Color3 GLfloat]
    -> Int -> Int -> [GLfloat]
createCandlestick bounds priceRange prices colors numElements index =
  [centerX, lowY] ++ colorList
      ++ [centerX, highY] ++ colorList
      ++ [leftX, openY] ++ colorList
      ++ [centerX, openY] ++ colorList
      ++ [centerX, closeY] ++ colorList
      ++ [rightX, closeY] ++ colorList
  where
    colorList = color3ToList $
        if index < length colors
          then colors !! index
          else
            if getPriceChange prices index >= 0
              then green3
              else red3

    (leftX, centerX, rightX) = getXValues bounds numElements index

    price = prices !! index
    lowY = getY bounds priceRange $ low price
    highY = getY bounds priceRange $ high price
    openY = getY bounds priceRange $ open price
    closeY = getY bounds priceRange $ close price

