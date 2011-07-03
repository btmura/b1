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
getMovingAverageLines priceData bounds movingAverageFunction color = lineStrip
  where
    priceRange = getPriceRange priceData
    numElements = numDailyElements priceData

    values = take numElements $ movingAverageFunction priceData
    percentages = map (realToFrac . heightPercentage priceRange) values

    indices = [0 .. numElements - 1]
    points = map (colorLineStripPoint bounds color percentages numElements)
        indices
    lineStrip = concat points


