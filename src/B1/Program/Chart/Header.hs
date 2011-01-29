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
  [symbolRight, symbolBottom, symbolLeft, symbolTop] <- prepareLayoutText
      resources fontSize headerLineLength symbol
  let symbolWidth = abs $ symbolRight - symbolLeft
      symbolHeight = abs $ symbolBottom - symbolTop
      headerHeight = padding + symbolHeight + padding
      buttonWidth = headerHeight
      buttonHeight = headerHeight
  preservingMatrix $ do
    translate $ vector3 padding (-padding - symbolHeight) 0
    color $ green alpha
    renderLayoutText resources symbol

  let status = getStatus maybePrices
      statusAlpha = fst $ current statusAlphaAnimation
      statusLineLength = headerLineLength - symbolWidth - buttonWidth
  [statusRight, statusBottom, statusLeft, statusTop] <- prepareLayoutText
      resources fontSize statusLineLength status
  preservingMatrix $ do
    translate $ vector3 (padding + symbolWidth) (-padding - symbolHeight) 0
    color $ green $ min alpha statusAlpha
    renderLayoutText resources status

  preservingMatrix $ do
    translate $ vector3 (width - buttonWidth) 0 0
    drawTextButton resources buttonWidth buttonHeight "+" alpha

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
    headerLineLength = realToFrac width
    padding = 10

getStatus :: Maybe Prices -> String

getStatus (Just (Just (todaysPrice:yesterdaysPrice:_), _)) = 
  printf "  %0.2f  %+0.2f" todaysClose todaysChange
  where
    todaysClose = close todaysPrice
    todaysChange = todaysClose - close yesterdaysPrice

getStatus (Just (Nothing, errors)) = "  [" ++ concat errors ++ "]"

getStatus Nothing = ""

drawTextButton :: Resources -> GLfloat -> GLfloat -> String -> GLfloat -> IO ()
drawTextButton resources width height text alpha = do
  preservingMatrix $ do
    [right, bottom, left, top] <- prepareLayoutText resources
        fontSize (realToFrac (width*2)) text

    let textWidth = abs $ right - left
        textHeight = abs $ bottom - top
        textTranslateX = width / 2 - textWidth / 2
        textTranslateY = (-height) / 2 - textHeight / 2

    translate $ vector3 textTranslateX textTranslateY 0
    color $ white alpha
    renderLayoutText resources text
  where
    fontSize = 18
    padding = 10
