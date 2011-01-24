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
  header <- getHeader symbol pricesMVar
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

getHeader :: String -> MVar PriceErrorTuple -> IO String
getHeader symbol priceErrorTupleMVar = do
  maybePriceErrorTuple <- tryTakeMVar priceErrorTupleMVar
  case maybePriceErrorTuple of
    Just priceErrorTuple -> do
      tryPutMVar priceErrorTupleMVar priceErrorTuple
      case priceErrorTuple of
        (Just (currentPrice:_), _) ->
          return $ printf "%s %0.2f" symbol (close currentPrice)
        (Nothing, (error:_)) ->
          return $ symbol ++ " [Error: " ++ error ++ "]"
        _ ->
          return $ symbol ++ " ?"
    _ -> return $ symbol

