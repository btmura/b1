module B1.Program.Chart.Header
  ( HeaderInput(..)
  , HeaderOutput(..)
  , HeaderOptions(..)
  , HeaderState
  , HeaderButton(..)
  , HeaderStatusStyle(..)
  , drawHeader
  , newHeaderState
  ) where

import Control.Monad
import Data.Maybe
import Data.Time
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import System.Locale
import Text.Printf

import B1.Data.Price
import B1.Data.Price.Google
import B1.Data.Range
import B1.Data.Symbol
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Button
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.MouseUtils
import B1.Program.Chart.Resources

data HeaderInput = HeaderInput
  { bounds :: Box
  , alpha :: GLfloat
  , symbol :: Symbol
  , stockData :: StockData
  , inputState :: HeaderState
  }

data HeaderOutput = HeaderOutput
  { outputState :: HeaderState
  , isDirty :: Dirty
  , height :: GLfloat
  , buttonClickedSymbol :: Maybe Symbol
  , statusClickedSymbol :: Maybe Symbol
  }

data HeaderOptions = HeaderOptions
  { fontSize :: Int
  , padding :: GLfloat
  , statusStyle :: HeaderStatusStyle
  , button :: HeaderButton
  }

data HeaderState = HeaderState
  { options :: HeaderOptions
  , getStatus :: Symbol -> StockData -> IO HeaderStatus
  , isStatusShowing :: Bool
  , statusAlphaAnimation :: Animation (GLfloat, Dirty)
  , maybeTextBounds :: Maybe Box
  }

data HeaderStatus = HeaderStatus String (GLfloat -> Color4 GLfloat)

data HeaderStatusStyle = ShortStatus | LongStatus

data HeaderButton = AddButton | RemoveButton deriving (Eq)

newHeaderState :: HeaderOptions -> HeaderState
newHeaderState options = HeaderState
  { options = options
  , getStatus = statusFunction
  , isStatusShowing = False
  , statusAlphaAnimation = animateOnce $ linearRange 0 1 30
  , maybeTextBounds = Nothing
  }
  where
    statusFunction = case statusStyle options of
      ShortStatus -> getShortStatus
      _ -> getLongStatus

-- | Draws header starting from the upper left corner to the bottom
-- right corner.
drawHeader :: Resources -> HeaderInput -> IO HeaderOutput
drawHeader resources@Resources
      { font = font
      , mousePosition = mousePosition
      }
    HeaderInput
      { bounds = bounds
      , alpha = alpha
      , symbol = symbol
      , stockData = stockData
      , inputState = inputState@HeaderState
        { options = HeaderOptions
          { button = button
          , fontSize = fontSize
          , padding = padding
          }
        , getStatus = getStatus
        , isStatusShowing = isStatusShowing
        , statusAlphaAnimation = statusAlphaAnimation
        , maybeTextBounds = maybeTextBounds
        }
      } = 
  preservingMatrix $ do
    translate $ vector3 (-(boxWidth bounds / 2)) (boxHeight bounds / 2) 0

    HeaderStatus statusText statusColor <- getStatus symbol stockData
    let statusTextSpec = headerTextSpec statusText
    textBounds <- maybe (measureText statusTextSpec) return maybeTextBounds

    let textHeight = boxHeight textBounds
        statusAlpha = fst $ current statusAlphaAnimation
        headerHeight = padding + textHeight + padding
        statusY = (-headerHeight - textHeight) / 2

    preservingMatrix $ do
      translate $ vector3 headerHeight statusY 0
      color $ statusColor $ min alpha statusAlpha
      renderText statusTextSpec

    -- TODO: Improve texture choosing to not hardcode numbers
    let buttonBounds = Box (boxLeft bounds, boxTop bounds)
            (boxLeft bounds + headerHeight, boxTop bounds - headerHeight)
        statusBounds = Box (boxRight buttonBounds, boxTop bounds)
            (boxRight bounds, boxTop bounds - headerHeight)
        buttonTextureNumber = if button == AddButton then 0 else 1

    buttonState <- preservingMatrix $ do
      translate $ vector3 (headerHeight / 2) (-headerHeight / 2) 0
      drawButton resources buttonBounds buttonTextureNumber alpha

    nextIsStatusShowing <- isStockPriceData stockData
    let nextStatusAlphaAnimation =
          (if nextIsStatusShowing then next else id) statusAlphaAnimation
        outputState = inputState
          { isStatusShowing = nextIsStatusShowing
          , statusAlphaAnimation = nextStatusAlphaAnimation
          , maybeTextBounds = Just textBounds
          }
        nextIsDirty = (snd . current) nextStatusAlphaAnimation
        nextButtonClickedSymbol =
            if buttonState == Clicked
              then Just symbol
              else Nothing
        nextStatusClickedSymbol =
            if isMouseClickedWithinBox resources statusBounds alpha
              then Just symbol
              else Nothing
    return HeaderOutput
      { outputState = outputState
      , isDirty = nextIsDirty
      , height = headerHeight
      , buttonClickedSymbol = nextButtonClickedSymbol
      , statusClickedSymbol = nextStatusClickedSymbol
      }

  where
    headerTextSpec = TextSpec font fontSize
    symbolTextSpec = headerTextSpec symbol

getLongStatus :: Symbol -> StockData -> IO HeaderStatus
getLongStatus = renderStatus renderLongStatus 
 
getShortStatus :: Symbol -> StockData -> IO HeaderStatus
getShortStatus = renderStatus renderShortStatus

renderStatus :: (Symbol -> [Price] -> HeaderStatus) -> Symbol -> StockData
    -> IO HeaderStatus
renderStatus renderFunction symbol stockData =
  handleStockData (return . renderFunction symbol . prices)
      (\_ -> return (renderEmptyStatus symbol))
      (HeaderStatus symbol green4) -- Doesn't render anything
      stockData

renderLongStatus :: Symbol -> [Price] -> HeaderStatus
renderLongStatus symbol prices =
  case prices of
    (todaysPrice:yesterdaysPrice:_) ->
      let todaysClose = close todaysPrice
          yesterdaysClose = close yesterdaysPrice
          todaysChange = todaysClose - yesterdaysClose
          percentChange = todaysChange / yesterdaysClose * 100
          statusText = printf "%s  %0.2f  %+0.2f  %+0.2f%%" symbol todaysClose
              todaysChange percentChange
          color = if todaysChange >= 0 then green4 else red4
      in HeaderStatus statusText color
    _ -> renderEmptyStatus symbol

renderShortStatus :: Symbol -> [Price] -> HeaderStatus
renderShortStatus symbol prices =
  case prices of
    (todaysPrice:yesterdaysPrice:_) ->
      let todaysClose = close todaysPrice
          todaysChange = todaysClose - close yesterdaysPrice
          statusText = printf "%s  %0.2f  %+0.2f" symbol todaysClose
              todaysChange
          color = if todaysChange >= 0 then green4 else red4
      in HeaderStatus statusText color
    _ -> renderEmptyStatus symbol

renderEmptyStatus :: Symbol -> HeaderStatus
renderEmptyStatus symbol = HeaderStatus symbol gray4

