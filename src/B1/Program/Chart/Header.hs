module B1.Program.Chart.Header
  ( HeaderInput(..)
  , HeaderOutput(..)
  , HeaderState
  , drawHeader
  , newHeaderState
  ) where

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
import B1.Program.Chart.Symbol

data HeaderInput = HeaderInput
  { width :: GLfloat
  , alpha :: GLfloat
  , symbol :: Symbol
  , maybePrices :: Maybe PriceErrorTuple
  , inputState :: HeaderState
  }

data HeaderOutput = HeaderOutput
  { outputState :: HeaderState
  , isDirty :: Bool
  , height :: GLfloat
  }

data HeaderState = HeaderState
  { isStatusShowing :: Bool
  , statusAlphaAnimation :: Animation (GLfloat, Dirty)
  }

newHeaderState :: HeaderState
newHeaderState = HeaderState
  { isStatusShowing = False
  , statusAlphaAnimation = animateOnce $ linearRange 0 1 30
  }

-- | Draws header starting from the upper left corner to the bottom
-- right corner.
drawHeader :: Resources -> HeaderInput -> IO HeaderOutput
drawHeader resources
    HeaderInput
      { width = width
      , alpha = alpha
      , symbol = symbol
      , maybePrices = maybePrices
      , inputState = inputState@HeaderState
        { isStatusShowing = isStatusShowing
        , statusAlphaAnimation = statusAlphaAnimation
        }
      } = do

  [right, bottom, left, top] <- prepareLayoutText resources
      fontSize layoutLineLength symbol

  let symbolWidth = abs $ right - left
      textHeight = abs $ bottom - top
      headerHeight = padding + textHeight + padding

  preservingMatrix $ do
    translate $ vector3 padding (-padding - textHeight) 0
    color $ green alpha
    renderLayoutText resources symbol

    let status = getStatus maybePrices
        statusAlpha = fst $ current statusAlphaAnimation

    translate $ vector3 symbolWidth 0 0
    color $ green $ min alpha statusAlpha
    renderLayoutText resources status

    let nextIsStatusShowing = isJust maybePrices
        nextStatusAlphaAnimation =
          (if nextIsStatusShowing then next else id) statusAlphaAnimation
        outputState = inputState
          { isStatusShowing = nextIsStatusShowing
          , statusAlphaAnimation = nextStatusAlphaAnimation
          }

    return HeaderOutput
      { outputState = outputState
      , isDirty = snd $ current nextStatusAlphaAnimation
      , height = headerHeight
      }

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
