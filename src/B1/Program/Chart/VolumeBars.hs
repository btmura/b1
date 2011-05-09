module B1.Program.Chart.VolumeBars
  ( VolumeBarsInput(..)
  , VolumeBarsOutput(..)
  , VolumeBarsState
  , drawVolumeBars
  , newVolumeBarsState
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Price
import B1.Data.Range
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.OpenGL.Vbo
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data VolumeBarsInput = VolumeBarsInput
  { bounds :: Box
  , alpha :: GLfloat
  , stockData :: StockData
  , inputState :: VolumeBarsState
  }

data VolumeBarsOutput = VolumeBarsOutput
  { outputState :: VolumeBarsState
  , isDirty :: Dirty
  }

data VolumeBarsState = VolumeBarsState
  { maybeVbo :: Maybe Vbo
  }

newVolumeBarsState :: VolumeBarsState
newVolumeBarsState = VolumeBarsState { maybeVbo = Nothing }

drawVolumeBars :: Resources -> VolumeBarsInput -> IO VolumeBarsOutput
drawVolumeBars resources
    input@VolumeBarsInput
      { stockData = stockData
      , inputState = state
      } = do
  maybePriceData <- getStockPriceData stockData
  case maybePriceData of
    Just priceDataOrError ->
      either (renderPriceData input)
          (renderError state)
          priceDataOrError
    _ -> renderNothing state

renderPriceData :: VolumeBarsInput -> StockPriceData -> IO VolumeBarsOutput
renderPriceData
    input@VolumeBarsInput
      { bounds = bounds
      , inputState = state@VolumeBarsState { maybeVbo = maybeVbo }
      }
    priceData = do

  vbo <- maybe (createVolumeBarsVbo priceData) return maybeVbo

  preservingMatrix $ do
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1
    render vbo

  return VolumeBarsOutput
    { outputState = state { maybeVbo = Just vbo }
    , isDirty = False
    }

createVolumeBarsVbo :: StockPriceData -> IO Vbo
createVolumeBarsVbo priceData = do
  bufferObject <- createBufferObject vertices
  return $ VertexVbo bufferObject Quads numElements
  where
    vertices = getVolumeBarQuads $ prices priceData
    numElements = length vertices `div` 2

getVolumeBarQuads :: [Price] -> [GLfloat]
getVolumeBarQuads prices =
  concat $ map (createQuad prices) [0 .. length prices - 1]

createQuad :: [Price] -> Int -> [GLfloat]
createQuad prices index =
  [ leftX, -1
  , leftX, topY
  , rightX, topY
  , rightX, -1
  ]
  where
    totalWidth = 2
    barWidth = realToFrac totalWidth / realToFrac (length prices)
    spacing = barWidth / 3
    rightX = totalWidth / 2 - realToFrac index * barWidth - spacing
    leftX = rightX - barWidth + spacing

    maxVolume = maximum $ map volume prices
    minVolume = minimum $ map volume prices
    totalRange = maxVolume - minVolume

    currentVolume = volume $ prices !! index
    range = currentVolume - minVolume
    heightPercentage = realToFrac range / realToFrac totalRange
    totalHeight = 2
    height = totalHeight * realToFrac heightPercentage
    topY = -(totalHeight / 2) + height

renderError :: VolumeBarsState -> String -> IO VolumeBarsOutput
renderError state errorMessage =
  return VolumeBarsOutput
    { outputState = state
    , isDirty = False
    }

renderNothing :: VolumeBarsState -> IO VolumeBarsOutput
renderNothing state =
  return VolumeBarsOutput
    { outputState = state
    , isDirty = False
    }

