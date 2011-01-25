module B1.Program.Chart.Chart
  ( ChartState(..)
  , Symbol
  , drawChart
  ) where

import Control.Concurrent.MVar
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Price
import B1.Data.Price.Google
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources

type Symbol = String

data ChartState = ChartState
  { chartWidth :: GLfloat -- ^ Width of the chart set by the caller.
  , chartHeight :: GLfloat -- ^ Height of the chart set by the caller.
  , chartAlpha :: GLfloat -- ^ Alpha of the chart set by the caller.
  , symbol :: String
  , pricesMVar :: MVar PriceErrorTuple
  }

drawChart :: Resources -> ChartState -> IO (ChartState, Dirty)
drawChart resources@Resources { layout = layout }
    state@ChartState
      { chartWidth = width
      , chartHeight = height
      , chartAlpha = alpha
      , symbol = symbol
      , pricesMVar = pricesMVar
      }  = do
  isDirty <- isEmptyMVar pricesMVar
  priceErrorTuple <- getPriceErrorTuple pricesMVar
  let header = getHeader symbol priceErrorTuple
  [left, bottom, right, top] <- prepareTextLayout resources fontSize
      layoutLineLength header

  let textHeight = abs $ bottom - top
      textCenterX = -(width / 2) + symbolPadding
      textCenterY = height / 2 - symbolPadding - textHeight

  color $ green alpha
  preservingMatrix $ do 
    translate $ vector3 textCenterX textCenterY 0
    renderLayout layout header

  return (state, isDirty)

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    symbolPadding = 15

getPriceErrorTuple :: MVar PriceErrorTuple -> IO (Maybe PriceErrorTuple)
getPriceErrorTuple pricesMVar = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just priceErrorTuple -> do
      tryPutMVar pricesMVar priceErrorTuple
      return $ Just priceErrorTuple
    _ -> return Nothing

getHeader :: String -> Maybe PriceErrorTuple -> String

getHeader symbol (Just (Just (todaysPrice:yesterdaysPrice:_), _)) = 
  printf "%s  %0.2f  %+0.2f" symbol (close todaysPrice) change
  where
    change = close todaysPrice - close yesterdaysPrice

getHeader symbol (Just (_, errors)) =
  symbol ++ " [Error: " ++ concat errors ++ "]"

getHeader symbol _ = symbol

