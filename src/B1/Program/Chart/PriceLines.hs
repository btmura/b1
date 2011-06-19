module B1.Program.Chart.PriceLines
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

numLines = 10

getVboSpecs :: StockPriceData -> Box -> [VboSpec]
getVboSpecs priceData bounds =
  [VboSpec Lines size elements]
  where
    size = numLines * (2 * floatsPerVertex)
    elements = getElements priceData bounds

getElements :: StockPriceData -> Box -> [GLfloat]
getElements priceData bounds = concat lines
  where
    lines = map (getLineFloats bounds)  [0 .. numLines - 1]

getLineFloats :: Box -> Int -> [GLfloat]
getLineFloats bounds@(Box (left, _) (right, bottom)) index =
  [left, y] ++ colorList
      ++ [right, y] ++ colorList
  where
    colorList = color3ToList darkBlue3
    increment = boxHeight bounds / realToFrac numLines
    y = bottom + (increment * realToFrac index)

