module B1.Program.Chart.Chart
  ( ChartState(..)
  , Symbol
  , drawChart
  , newHeaderState
  ) where

import Control.Concurrent.MVar
import Data.Maybe
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

type Symbol = String

data ChartState = ChartState
  { chartWidth :: GLfloat -- ^ Width of the chart set by the caller.
  , chartHeight :: GLfloat -- ^ Height of the chart set by the caller.
  , chartAlpha :: GLfloat -- ^ Alpha of the chart set by the caller.
  , symbol :: String
  , pricesMVar :: MVar PriceErrorTuple
  , headerState :: HeaderState
  }

drawChart :: Resources -> ChartState -> IO (ChartState, Dirty)
drawChart resources@Resources { layout = layout }
    state@ChartState
      { chartWidth = width
      , chartHeight = height
      , chartAlpha = alpha
      , symbol = symbol
      , pricesMVar = pricesMVar
      , headerState = headerState
      }  = do
  isPricesDirty <- isEmptyMVar pricesMVar
  maybePrices <- getPriceErrorTuple pricesMVar

  preservingMatrix $ do
    -- Start from the upper left corner
    translate $ vector3 (-(width / 2)) (height / 2) 0

    (nextHeaderState, isHeaderDirty) <- drawHeader resources state headerState
        maybePrices

    let nextState = state { headerState = nextHeaderState }
        isDirty = isPricesDirty || isHeaderDirty
    return (nextState, isDirty)

getPriceErrorTuple :: MVar PriceErrorTuple -> IO (Maybe PriceErrorTuple)
getPriceErrorTuple pricesMVar = do
  maybePrices <- tryTakeMVar pricesMVar
  case maybePrices of
    Just priceErrorTuple -> do
      tryPutMVar pricesMVar priceErrorTuple
      return $ Just priceErrorTuple
    _ -> return Nothing

data HeaderState = HeaderState
  { isStatusShowing :: Bool
  , statusAlphaAnimation :: Animation (GLfloat, Dirty)
  }

newHeaderState :: HeaderState
newHeaderState = HeaderState
  { isStatusShowing = False
  , statusAlphaAnimation = animateOnce $ linearRange 0 1 30
  }

drawHeader :: Resources -> ChartState -> HeaderState
    -> Maybe PriceErrorTuple -> IO (HeaderState, Dirty)
drawHeader resources@Resources { layout = layout }
    ChartState
      { chartWidth = width
      , chartAlpha = alpha
      , symbol = symbol 
      }
    headerState@HeaderState
      { isStatusShowing = isStatusShowing
      , statusAlphaAnimation = statusAlphaAnimation
      }
    maybePrices = do

  [symbolLeft, bottom, symbolRight, top] <- prepareTextLayout resources
      fontSize layoutLineLength symbol

  let symbolWidth = abs $ symbolRight - symbolLeft 
      textHeight = abs $ bottom - top
      headerHeight = padding + textHeight + padding
      status = getStatus maybePrices
      statusAlpha = fst $ getCurrentFrame statusAlphaAnimation

  preservingMatrix $ do
    translate $ vector3 padding (-padding - textHeight) 0
    color $ green alpha
    renderLayout layout symbol

    translate $ vector3 symbolWidth 0 0
    color $ green $ min alpha statusAlpha
    renderLayout layout status

    let nextIsStatusShowing = isJust maybePrices
        nextStatusAlphaAnimation = if nextIsStatusShowing
          then getNextAnimation statusAlphaAnimation
          else statusAlphaAnimation
        nextHeaderState = headerState
          { isStatusShowing = nextIsStatusShowing
          , statusAlphaAnimation = nextStatusAlphaAnimation
          }
    return (nextHeaderState, snd $ getCurrentFrame nextStatusAlphaAnimation)

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    padding = 10

getStatus :: Maybe PriceErrorTuple -> String

getStatus (Just (Just (todaysPrice:yesterdaysPrice:_), _)) = 
  printf "  %0.2f  %+0.2f" todaysClose todaysChange
  where
    todaysClose = close todaysPrice
    todaysChange = todaysClose - close yesterdaysPrice

getStatus _ = ""
