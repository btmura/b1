module B1.Program.Chart.Header
  ( HeaderInput(..)
  , HeaderOutput(..)
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
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data HeaderInput = HeaderInput
  { bounds :: Box
  , fontSize :: FontSize
  , padding :: GLfloat
  , alpha :: GLfloat
  , symbol :: Symbol
  , stockData :: StockData
  , inputState :: HeaderState
  }

data HeaderOutput = HeaderOutput
  { outputState :: HeaderState
  , isDirty :: Dirty
  , height :: GLfloat
  , clickedSymbol :: Maybe Symbol
  }

data HeaderState = HeaderState
  { button :: HeaderButton
  , getStatus :: Symbol -> StockData -> IO HeaderStatus
  , isStatusShowing :: Bool
  , statusAlphaAnimation :: Animation (GLfloat, Dirty)
  }

data HeaderStatus = HeaderStatus String (GLfloat -> Color4 GLfloat)

data HeaderStatusStyle = ShortStatus | LongStatus

data HeaderButton = AddButton | RemoveButton

newHeaderState :: HeaderStatusStyle -> HeaderButton -> HeaderState
newHeaderState statusStyle button = HeaderState
  { button = button
  , getStatus = statusFunction
  , isStatusShowing = False
  , statusAlphaAnimation = animateOnce $ linearRange 0 1 30
  }
  where
    statusFunction = case statusStyle of
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
      , fontSize = fontSize
      , padding = padding
      , alpha = alpha
      , symbol = symbol
      , stockData = stockData
      , inputState = inputState@HeaderState
        { button = button
        , getStatus = getStatus
        , isStatusShowing = isStatusShowing
        , statusAlphaAnimation = statusAlphaAnimation
        }
      } = 
  preservingMatrix $ do
    translate $ vector3 (-(boxWidth bounds / 2)) (boxHeight bounds / 2) 0

    HeaderStatus statusText statusColor <- getStatus symbol stockData
    let statusTextSpec = headerTextSpec statusText
    statusBox <- measureText statusTextSpec

    let statusHeight = boxHeight statusBox
        statusAlpha = fst $ current statusAlphaAnimation
        headerHeight = padding + statusHeight + padding
        statusY = (-headerHeight - statusHeight) / 2

    preservingMatrix $ do
      translate $ vector3 headerHeight statusY 0
      color $ statusColor $ min alpha statusAlpha
      renderText statusTextSpec

    -- TODO: Improve texture choosing to not hardcode numbers
    let buttonHitBox = Box (boxLeft bounds, boxTop bounds)
            (boxLeft bounds + headerHeight, boxTop bounds - headerHeight)
        buttonHover = alpha >= 1
            && boxContains buttonHitBox mousePosition
        buttonClicked = buttonHover
            && isMouseButtonClicked resources ButtonLeft
        (click, hover, normal) =
          case button of
            AddButton -> (1, 1, 0)
            RemoveButton -> (3, 3, 2)
        buttonTextureNumber
          | buttonClicked = click
          | buttonHover = hover
          | otherwise = normal

    preservingMatrix $ do
      translate $ vector3 (headerHeight / 2) (-headerHeight / 2) 0
      drawHeaderButton headerHeight headerHeight buttonTextureNumber alpha

    maybePriceData <- getStockPriceData stockData
    let nextIsStatusShowing = isJust maybePriceData
        nextStatusAlphaAnimation =
          (if nextIsStatusShowing then next else id) statusAlphaAnimation
        outputState = inputState
          { isStatusShowing = nextIsStatusShowing
          , statusAlphaAnimation = nextStatusAlphaAnimation
          }
        nextIsDirty = buttonClicked || (snd . current) nextStatusAlphaAnimation
        nextClickedSymbol = if buttonClicked then Just symbol else Nothing

    return HeaderOutput
      { outputState = outputState
      , isDirty = nextIsDirty
      , height = headerHeight
      , clickedSymbol = nextClickedSymbol
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
renderStatus renderFunction symbol stockData = do
  maybePriceData <- getStockPriceData stockData
  return $ maybe (HeaderStatus symbol green)
      (either (renderFunction symbol . prices)
          (\_ -> renderEmptyStatus symbol))
      maybePriceData

renderLongStatus :: Symbol -> [Price] -> HeaderStatus
renderLongStatus symbol prices =
  case prices of
    (todaysPrice:yesterdaysPrice:_) ->
      let todaysClose = close todaysPrice
          todaysChange = todaysClose - close yesterdaysPrice
          date = formatTime defaultTimeLocale "%-m/%-d/%y"
              (endTime todaysPrice)
          statusText = printf "%s  %0.2f  %+0.2f  %s" symbol todaysClose
              todaysChange date
          color = if todaysChange >= 0 then green else red
      in HeaderStatus statusText color
    _ -> renderEmptyStatus symbol

renderShortStatus :: Symbol -> [Price] -> HeaderStatus
renderShortStatus symbol prices =
  case prices of
    (todaysPrice:yesterdaysPrice:_) ->
      let todaysClose = close todaysPrice
          todaysChange = todaysClose - close yesterdaysPrice
          statusText = printf "%s %0.2f  %+0.2f" symbol todaysClose
              todaysChange
          color = if todaysChange >= 0 then green else red
      in HeaderStatus statusText color
    _ -> renderEmptyStatus symbol

renderEmptyStatus :: Symbol -> HeaderStatus
renderEmptyStatus symbol = 
  let statusText = printf "%s  -  -  -" symbol
  in HeaderStatus statusText gray

drawHeaderButton :: GLfloat -> GLfloat -> Int -> GLfloat -> IO ()
drawHeaderButton width height textureNumber alpha = 
  preservingMatrix $ do
    texture Texture2D $= Enabled
    textureBinding Texture2D $= Just (TextureObject 
        (fromIntegral textureNumber))
    scale3 (width / 2) (height / 2) 1
    color $ white alpha
    renderPrimitive Quads $ do
      normal $ normal3 0 0 1
      texCoord $ texCoord2 0 0
      vertex $ vertex2 (-1) (-1)
      texCoord $ texCoord2 0 1
      vertex $ vertex2 (-1) 1
      texCoord $ texCoord2 1 1
      vertex $ vertex2 1 1
      texCoord $ texCoord2 1 0
      vertex $ vertex2 1 (-1)
    texture Texture2D $= Disabled

