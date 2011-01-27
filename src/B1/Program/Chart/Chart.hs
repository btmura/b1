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
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.Header as H

data ChartInput = ChartInput
  { width :: GLfloat
  , height :: GLfloat
  , alpha :: GLfloat
  , symbol :: Symbol
  , inputState :: ChartState
  }

data ChartOutput = ChartOutput
  { outputState :: ChartState
  , isDirty :: Dirty
  }

data ChartState = ChartState
  { pricesMVar :: MVar PriceErrorTuple
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
      { width = width
      , height = height
      , alpha = alpha
      , symbol = symbol
      , inputState = inputState@ChartState
        { pricesMVar = pricesMVar
        , headerState = headerState
        }
      }  = do
  isPricesDirty <- isEmptyMVar pricesMVar
  maybePrices <- getPriceErrorTuple pricesMVar

  preservingMatrix $ do
    -- Start from the upper left corner
    translate $ vector3 (-(width / 2)) (height / 2) 0

    let headerInput = H.HeaderInput
          { H.width = width
          , H.alpha = alpha
          , H.symbol = symbol
          , H.maybePrices = maybePrices
          , H.inputState = headerState
          }

    headerOutput <- H.drawHeader resources headerInput 

    let outputState = inputState { headerState = H.outputState headerOutput }
        isDirty = isPricesDirty || H.isDirty headerOutput
    return ChartOutput
      { outputState = outputState
      , isDirty = isDirty
      }

getPriceErrorTuple :: MVar PriceErrorTuple -> IO (Maybe PriceErrorTuple)
getPriceErrorTuple pricesMVar = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just priceErrorTuple -> do
      tryPutMVar pricesMVar priceErrorTuple
      return $ Just priceErrorTuple
    _ -> return Nothing

