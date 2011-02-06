module B1.Program.Chart.Chart
  ( ChartInput(..)
  , ChartOutput(..)
  , ChartState
  , Symbol
  , drawChart
  , newChartState
  ) where

import Control.Concurrent 
import Control.Concurrent.MVar
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Graphics.Rendering.FTGL
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Range
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
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
  }

data ChartState = ChartState
  { pricesMVar :: MVar Prices
  , headerState :: H.HeaderState
  }

newChartState :: Symbol -> IO ChartState
newChartState symbol = do
  pricesMVar <- newEmptyMVar
  forkIO $ do
    startDate <- getStartDate
    endDate <- getEndDate 
    prices <- getGooglePrices startDate endDate symbol
    putMVar pricesMVar prices
  return $ ChartState
    { pricesMVar = pricesMVar
    , headerState = H.newHeaderState
    }

getStartDate :: IO LocalTime
getStartDate = do
  endDate <- getEndDate
  let yearAgo = addGregorianYearsClip (-1) (localDay endDate)
  return endDate
    { localDay = yearAgo
    , localTimeOfDay = midnight
    }

getEndDate :: IO LocalTime
getEndDate = do
  timeZone <- getCurrentTimeZone
  time <- getCurrentTime
  let localTime = utcToLocalTime timeZone time 
  return $ localTime { localTimeOfDay = midnight }

drawChart :: Resources -> ChartInput -> IO ChartOutput
drawChart resources
    ChartInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , inputState = inputState@ChartState
        { pricesMVar = pricesMVar
        , headerState = headerState
        }
      }  = do
  isPricesDirty <- isEmptyMVar pricesMVar
  maybePrices <- getPrices pricesMVar

  preservingMatrix $ do
    -- Start from the upper left corner
    translate $ vector3 (-(boxWidth bounds / 2)) (boxHeight bounds / 2) 0

    let headerInput = H.HeaderInput
          { H.bounds = bounds
          , H.alpha = alpha
          , H.symbol = symbol
          , H.maybePrices = maybePrices
          , H.inputState = headerState
          }

    headerOutput <- H.drawHeader resources headerInput 

    let H.HeaderOutput
          { H.outputState = outputHeaderState
          , H.isDirty = isHeaderDirty
          , H.height = headerHeight
          } = headerOutput

    -- Draw a line under the header
    translate $ vector3 0 (-headerHeight) 0
    drawDivider (boxWidth bounds) alpha

    let outputState = inputState { headerState = outputHeaderState }
        isDirty = isPricesDirty || isHeaderDirty
    return ChartOutput
      { outputState = outputState
      , isDirty = isDirty
      }

getPrices :: MVar Prices -> IO (Maybe Prices)
getPrices pricesMVar = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just prices -> do
      tryPutMVar pricesMVar prices
      return $ Just prices
    _ -> return Nothing

drawDivider :: GLfloat -> GLfloat -> IO ()
drawDivider width alpha = do
  color $ blue alpha 
  renderPrimitive Lines $ do
    vertex $ vertex2 0 0
    vertex $ vertex2 width 0


