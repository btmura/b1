module B1.Program.Chart.Header
  ( HeaderInput(..)
  , HeaderOutput(..)
  , HeaderState
  , drawHeader
  , newHeaderState
  ) where

import Data.Maybe
import Data.Time
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import System.Locale
import Text.Printf

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Range
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

data HeaderInput = HeaderInput
  { width :: GLfloat
  , alpha :: GLfloat
  , symbol :: Symbol
  , maybePrices :: Maybe Prices
  , inputState :: HeaderState
  }

data HeaderOutput = HeaderOutput
  { outputState :: HeaderState
  , isDirty :: Dirty
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

  symbolBoundingBox <- measureText symbolTextSpec
  statusBoundingBox <- measureText statusTextSpec

  let symbolWidth = boxWidth symbolBoundingBox
      symbolHeight = boxHeight symbolBoundingBox

      statusHeight = boxHeight symbolBoundingBox
      statusAlpha = fst $ current statusAlphaAnimation

      headerHeight = padding + (max symbolHeight statusHeight) + padding
      symbolY = (-headerHeight - symbolHeight) / 2
      statusY = (-headerHeight - statusHeight) / 2

  preservingMatrix $ do
    translate $ vector3 padding symbolY 0
    color $ green alpha
    renderText symbolTextSpec

  preservingMatrix $ do
    translate $ vector3 (padding + symbolWidth) statusY 0
    color $ green $ min alpha statusAlpha
    renderText statusTextSpec

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
    headerTextSpec = TextSpec (font resources) 18
    symbolTextSpec = headerTextSpec symbol
    statusTextSpec = headerTextSpec $ getStatus maybePrices
    padding = 10

getStatus :: Maybe Prices -> String

getStatus (Just (Just (todaysPrice:yesterdaysPrice:_), _)) = 
  printf "  %0.2f  %+0.2f  %s" todaysClose todaysChange date
  where
    todaysClose = close todaysPrice
    todaysChange = todaysClose - close yesterdaysPrice
    date = formatTime defaultTimeLocale "%-m/%-d/%y" (endTime todaysPrice)

getStatus (Just (Nothing, errors)) = "  [" ++ concat errors ++ "]"

getStatus Nothing = ""

