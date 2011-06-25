module B1.Program.Chart.Overlay
  ( OverlayInput(..)
  , OverlayOutput(..)
  , OverlayState
  , OverlayOptions(..)
  , OverlayBoundSet(..)
  , drawOverlay
  , newOverlayState
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Price
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data OverlayInput = OverlayInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: OverlayState
  }

data OverlayOutput = OverlayOutput
  { outputState :: OverlayState
  , isDirty :: Dirty
  }

data OverlayState = OverlayState
  { options :: OverlayOptions
  , stockData :: StockData
  }

data OverlayOptions = OverlayOptions
  { boundSet :: OverlayBoundSet
  }

data OverlayBoundSet = OverlayBoundSet

newOverlayState :: OverlayOptions -> StockData -> OverlayState
newOverlayState options stockData = OverlayState
  { options = options
  , stockData = stockData
  }

drawOverlay :: Resources -> OverlayInput -> IO OverlayOutput
drawOverlay resources
    input@OverlayInput
      { bounds = bounds
      , alpha = alpha
      , inputState = inputState@OverlayState
        { stockData = stockData
        }
      } = do
  handleStockData (renderOverlay resources bounds alpha)
      (\_ -> return ()) () stockData
  return OverlayOutput
    { outputState = inputState
    , isDirty = False
    }

renderOverlay :: Resources -> Box -> GLfloat -> StockPriceData -> IO ()
renderOverlay resources bounds alpha priceData =
  when (alpha > 0 && boxContains bounds (mousePosition resources)) $ do
    renderCrosshair resources bounds alpha
    renderPriceInfo resources bounds alpha priceData

renderCrosshair :: Resources -> Box -> GLfloat -> IO ()
renderCrosshair resources bounds alpha =
  preservingMatrix $ do
    translateToWindowLowerLeft bounds

    let (mouseX, mouseY) = mousePosition resources
        lineColor4 = red4
    renderPrimitive Lines $ do
      -- Vertical line
      color $ lineColor4 0 
      vertex $ vertex2 mouseX (boxBottom bounds)

      color $ lineColor4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 0
      vertex $ vertex2 mouseX (boxTop bounds)

      -- Horizontal line
      color $ lineColor4 0 
      vertex $ vertex2 (boxLeft bounds) mouseY

      color $ lineColor4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 0
      vertex $ vertex2 (boxRight bounds) mouseY

renderPriceInfo :: Resources -> Box -> GLfloat -> StockPriceData -> IO ()
renderPriceInfo resources bounds alpha priceData = do
  let maybePrice = getPriceForMousePosition resources bounds priceData
  case maybePrice of
    Just price -> renderPriceText resources bounds alpha price
    _ -> return ()

getPriceForMousePosition :: Resources -> Box -> StockPriceData -> Maybe Price
getPriceForMousePosition resources bounds priceData
  | mouseX < left = Nothing
  | mouseX >= right = Nothing
  | otherwise = Just price
  where
    (mouseX, _) = mousePosition resources
    Box (left, _) (right, _) = bounds
    numElements = numDailyElements priceData
    elementWidth = boxWidth bounds / realToFrac numElements
    index = floor $ (mouseX - left) / elementWidth
    reverseIndex = numElements - 1 - index
    price = prices priceData !! reverseIndex

renderPriceText :: Resources -> Box -> GLfloat -> Price -> IO ()
renderPriceText resources bounds alpha price = do
  let windowPadding = 10
      bubblePadding = 7
      lineSpacing = 3

      textSpec = TextSpec (font resources) 12
      printFloat = printf "%+.2f"
      openText = "Open: " ++ (printFloat . open) price
      closeText = "Close: " ++ (printFloat . close) price
      highText = "High: " ++ (printFloat . high) price
      lowText = "Low: " ++ (printFloat . low) price

      printVolume = printf "%dK"
      reduceVolume volume = volume `div` 1000
      volumeText = "Volume: " ++ (printVolume . reduceVolume . volume) price

  openTextBox <- measureText $ textSpec openText
  closeTextBox <- measureText $ textSpec closeText
  highTextBox <- measureText $ textSpec highText
  lowTextBox <- measureText $ textSpec lowText
  volumeTextBox <- measureText $ textSpec volumeText

  let textItems = [closeText, openText, highText, lowText, volumeText]
      textBoxes = [closeTextBox, openTextBox, highTextBox, lowTextBox,
          volumeTextBox]

      largestTextWidth = maximum $ map boxWidth textBoxes
      bubbleWidth = bubblePadding + largestTextWidth + bubblePadding

      totalTextHeight = sum $ map ((+) lineSpacing . boxHeight) textBoxes
      bubbleHeight = bubblePadding + totalTextHeight + bubblePadding

  preservingMatrix $ do
    translate $ vector3 (-boxWidth bounds / 2 + windowPadding)
        (boxHeight bounds / 2 - windowPadding) 0

    preservingMatrix $ do
      color $ red4 alpha
      translate $ vector3 (bubbleWidth / 2) (-bubbleHeight / 2) 0
      drawRectangle bubbleWidth bubbleHeight bubblePadding

    color $ yellow4 alpha
    translate $ vector3 bubblePadding (-bubblePadding) 0
    mapM_ (\(textBox, text) -> do
        translate $ vector3 0 (-boxHeight textBox) 0
        renderText $ textSpec text
        translate $ vector3 0 (-lineSpacing) 0
        ) (zip textBoxes textItems)

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0

