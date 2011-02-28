module B1.Program.Chart.Chart
  ( ChartInput(..)
  , ChartOutput(..)
  , ChartState
  , Symbol
  , drawChart
  , newChartState
  ) where

import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Range
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.StockData
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.Header as H

data ChartInput = ChartInput
  { bounds :: Box
  , alpha :: GLfloat
  , symbol :: Symbol
  , inputState :: ChartState
  }

data ChartOutput = ChartOutput
  { outputState :: ChartState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol
  }

data ChartState = ChartState
  { stockData :: StockData
  , headerState :: H.HeaderState
  }

newChartState :: Symbol -> IO ChartState
newChartState symbol = do
  stockData <- newStockData symbol
  return $ ChartState
    { stockData = stockData
    , headerState = H.newHeaderState H.LongStatus H.AddButton
    }

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources
    ChartInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , inputState = inputState@ChartState
        { stockData = stockData
        , headerState = headerState
        }
      }  = 
  preservingMatrix $ do
    let headerInput = H.HeaderInput
          { H.bounds = bounds
          , H.fontSize = 18
          , H.padding = 10
          , H.alpha = alpha
          , H.symbol = symbol
          , H.stockData = stockData
          , H.inputState = headerState
          }

    headerOutput <- H.drawHeader resources headerInput 

    let H.HeaderOutput
          { H.outputState = outputHeaderState
          , H.isDirty = isHeaderDirty
          , H.height = headerHeight
          , H.clickedSymbol = addedSymbol
          } = headerOutput

    -- Draw a line under the header
    translate $ vector3 0 (boxHeight bounds / 2 - headerHeight) 0
    color $ outlineColor resources bounds alpha 
    drawHorizontalRule (boxWidth bounds - 1) 

    let outputState = inputState { headerState = outputHeaderState }
        isDirty = isHeaderDirty
    return ChartOutput
      { outputState = outputState
      , isDirty = isDirty
      , addedSymbol = addedSymbol
      }

