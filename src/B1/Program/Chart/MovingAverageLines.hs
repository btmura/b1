module B1.Program.Chart.MovingAverageLines
  ( getVboSpecs
  ) where
  
import Graphics.Rendering.OpenGL

import B1.Data.Technicals.MovingAverage
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Program.Chart.Colors
import B1.Program.Chart.GraphUtils
import B1.Program.Chart.Vbo

getVboSpecs :: StockPriceData -> Box -> [VboSpec]
getVboSpecs priceData bounds =
  map (uncurry (createMovingAverageVboSpec priceData bounds)) 
      movingAverageFunctions
  where
    movingAverageFunctions =
      [ (movingAverage25, purple3)
      , (movingAverage50, yellow3)
      , (movingAverage200, white3)
      ]

createMovingAverageVboSpec :: StockPriceData -> Box
    -> (StockPriceData -> [MovingAverage]) -> Color3 GLfloat -> VboSpec
createMovingAverageVboSpec priceData bounds movingAverageFunction color =
  VboSpec LineStrip size elements
  where
    size = getMovingAverageSize priceData movingAverageFunction
    elements = getMovingAverageLines priceData bounds
        movingAverageFunction color

getMovingAverageSize :: StockPriceData
    -> (StockPriceData -> [MovingAverage]) -> Int
getMovingAverageSize priceData movingAverageFunction =
  getLineSize $ trim $ movingAverageFunction priceData
  where
    trim = take $ numDailyElements priceData

getLineSize :: [a] -> Int
getLineSize list = size
  where
    numElements = length list
    floatsPerVertex = 2 + 3 -- x, y, and 3 for color
    size = numElements * floatsPerVertex

getMovingAverageLines :: StockPriceData -> Box
    -> (StockPriceData -> [MovingAverage]) -> Color3 GLfloat -> [GLfloat]
getMovingAverageLines priceData bounds movingAverageFunction color =
  concat lines
  where
    priceRange = getPriceRange priceData
    numElements = numDailyElements priceData
    trim = take numElements
    indices = [0 .. numElements - 1]
    values = trim $ movingAverageFunction priceData
    lines = map (createMovingAverageLine bounds priceRange
        values color numElements) indices

createMovingAverageLine :: Box -> (Float, Float) -> [MovingAverage]
    -> Color3 GLfloat -> Int -> Int -> [GLfloat]
createMovingAverageLine bounds priceRange movingAverages color numElements index
  | index >= length movingAverages = []
  | otherwise = [rightX, rightY] ++ colorList
  where
    colorList = color3ToList color
    (leftX, _, rightX) = getXValues bounds numElements index
    movingAverageValue = movingAverages !! index
    rightY = getY bounds priceRange movingAverageValue


