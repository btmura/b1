module B1.Program.Chart.Header
  ( HeaderInput(..)
  , HeaderOutput(..)
  , HeaderState
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
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.StockData
import B1.Program.Chart.Symbol

data HeaderInput = HeaderInput
  { bounds :: Box
  , fontSize :: FontSize
  , alpha :: GLfloat
  , symbol :: Symbol
  , stockData :: StockData
  , inputState :: HeaderState
  }

data HeaderOutput = HeaderOutput
  { outputState :: HeaderState
  , isDirty :: Dirty
  , height :: GLfloat
  , addedSymbol :: Maybe Symbol
  }

data HeaderState = HeaderState
  { getStatus :: StockData -> IO String
  , isStatusShowing :: Bool
  , statusAlphaAnimation :: Animation (GLfloat, Dirty)
  }

data HeaderStatusStyle = ShortStatus | LongStatus

newHeaderState :: HeaderStatusStyle -> HeaderState
newHeaderState statusStyle = HeaderState
  { getStatus =
      case statusStyle of
        ShortStatus -> getShortStatus
        _ -> getLongStatus
  , isStatusShowing = False
  , statusAlphaAnimation = animateOnce $ linearRange 0 1 30
  }

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
      , alpha = alpha
      , symbol = symbol
      , stockData = stockData
      , inputState = inputState@HeaderState
        { getStatus = getStatus
        , isStatusShowing = isStatusShowing
        , statusAlphaAnimation = statusAlphaAnimation
        }
      } = 
  preservingMatrix $ do
    loadIdentity
    translateToCenter bounds
    translate $ vector3 (-(boxWidth bounds / 2)) (boxHeight bounds / 2) 0

    symbolBox <- measureText symbolTextSpec

    statusText <- getStatus stockData
    let statusTextSpec = headerTextSpec statusText
    statusBox <- measureText statusTextSpec

    let symbolWidth = boxWidth symbolBox
        symbolHeight = boxHeight symbolBox

        statusHeight = boxHeight symbolBox
        statusAlpha = fst $ current statusAlphaAnimation

        textHeight = max symbolHeight statusHeight
        headerHeight = padding + textHeight + padding
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

    -- TODO: Improve texture choosing...
    let addHitBox = Box (boxRight bounds - headerHeight, boxTop bounds)
            (boxRight bounds, boxTop bounds - headerHeight)
        addButtonHover = alpha >= 1 && boxContains addHitBox mousePosition
        addButtonClicked = addButtonHover
            && isMouseButtonClicked resources ButtonLeft
        addTextureNumber = if addButtonClicked
            then 2
            else if addButtonHover then 1 else 0

    preservingMatrix $ do
      translate $ vector3 (boxWidth bounds - headerHeight / 2)
          (-headerHeight / 2) 0
      drawHeaderButton headerHeight headerHeight addTextureNumber alpha

    maybePriceData <- getStockPriceData stockData
    let nextIsStatusShowing = isJust maybePriceData
        nextStatusAlphaAnimation =
          (if nextIsStatusShowing then next else id) statusAlphaAnimation
        outputState = inputState
          { isStatusShowing = nextIsStatusShowing
          , statusAlphaAnimation = nextStatusAlphaAnimation
          }
        nextIsDirty = snd $ current nextStatusAlphaAnimation
        nextAddedSymbol = if addButtonClicked then Just symbol else Nothing

    return HeaderOutput
      { outputState = outputState
      , isDirty = nextIsDirty
      , height = headerHeight
      , addedSymbol = nextAddedSymbol
      }

  where
    headerTextSpec = TextSpec font fontSize
    symbolTextSpec = headerTextSpec symbol
    padding = 10

getLongStatus :: StockData -> IO String
getLongStatus = renderStatus renderLongStatus
 
getShortStatus :: StockData -> IO String
getShortStatus = renderStatus renderShortStatus

renderStatus :: (StockPriceData -> String) -> StockData -> IO String
renderStatus renderFunction stockData = do
  maybePriceData <- getStockPriceData stockData
  return $ case maybePriceData of
    (Just priceData) -> renderFunction priceData
    _ -> ""

renderLongStatus :: StockPriceData -> String
renderLongStatus priceData =
  case prices priceData of
    (Just (todaysPrice:yesterdaysPrice:_), _) ->
        let todaysClose = close todaysPrice
            todaysChange = todaysClose - close yesterdaysPrice
            date = formatTime defaultTimeLocale "%-m/%-d/%y"
                (endTime todaysPrice)
        in printf "  %0.2f  %+0.2f  %s" todaysClose todaysChange date
    _ -> "  -  -  -"

renderShortStatus :: StockPriceData -> String
renderShortStatus priceData =
  case prices priceData of
    (Just (todaysPrice:yesterdaysPrice:_), _) ->
        let todaysClose = close todaysPrice
            todaysChange = todaysClose - close yesterdaysPrice
        in printf "  %0.2f  %+0.2f" todaysClose todaysChange
    _ -> "  -  -" 

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

