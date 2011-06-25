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
  let padding = 10
      textSpec = TextSpec (font resources) 14
      printFunc = printf "%+.2f"
      openText = " Open: " ++ (printFunc . open) price
      closeText = " Close: " ++ (printFunc . close) price
      highText = " High: " ++ (printFunc . high) price
      lowText = " Low: " ++ (printFunc . low) price
  openTextBox <- measureText $ textSpec openText
  preservingMatrix $ do
    color $ yellow4 alpha
    translate $ vector3 (-boxWidth bounds / 2 + padding)
        (boxHeight bounds / 2 - padding - boxHeight openTextBox) 0
    renderText $ textSpec openText

    translate $ vector3 0 (-boxHeight openTextBox) 0
    renderText $ textSpec closeText

    translate $ vector3 0 (-boxHeight openTextBox) 0
    renderText $ textSpec highText

    translate $ vector3 0 (-boxHeight openTextBox) 0
    renderText $ textSpec lowText

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0

