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
import Data.Maybe
import Data.Time.Format
import Graphics.Rendering.OpenGL
import System.Locale
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
import B1.Program.Chart.GraphUtils
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
  { graphBounds :: Maybe Box
  , volumeBounds :: Maybe Box
  , stochasticBounds :: Maybe Box
  , weeklyStochasticBounds :: Maybe Box
  }

newOverlayState :: OverlayOptions -> StockData -> OverlayState
newOverlayState options stockData = OverlayState
  { options = options
  , stockData = stockData
  }

drawOverlay :: Resources -> OverlayInput -> IO OverlayOutput
drawOverlay resources
    input@OverlayInput
      { inputState = inputState@OverlayState
        { stockData = stockData
        }
      } = do
  handleStockData (renderOverlay resources input)
      (\_ -> return ()) () stockData
  return OverlayOutput
    { outputState = inputState
    , isDirty = False
    }

renderOverlay :: Resources -> OverlayInput -> StockPriceData -> IO ()
renderOverlay resources
    input@OverlayInput
      { bounds = bounds
      , alpha = alpha
      , inputState = OverlayState
        { options = OverlayOptions
          { boundSet = boundSet
          }
        }
      }
    priceData = do
  when (alpha >= 1 -- Too expensive to draw the overlay while animating
      && not (isMouseDrag resources)
      && boxContains bounds (mousePosition resources)) $ do
    let maybeTextX = getHorizontalAxisText resources bounds boundSet priceData
        maybeTextY = getVerticalAxisText resources bounds priceData
    renderCrosshair resources bounds maybeTextX maybeTextY
    renderPriceInfoBox resources bounds priceData

getHorizontalAxisText :: Resources -> Box -> OverlayBoundSet -> StockPriceData
    -> Maybe String
getHorizontalAxisText resources bounds 
    OverlayBoundSet
      { graphBounds = graphBounds
      , volumeBounds = volumeBounds
      , stochasticBounds = stochasticBounds
      , weeklyStochasticBounds = weeklyStochasticBounds
      }
    priceData =
  let labelGroups =
        [ (graphBounds, getPriceText)
        , (volumeBounds, getVolumeText)
        , (stochasticBounds, getStochasticText)
        , (weeklyStochasticBounds, getStochasticText)
        ]

      filterLabelGroup :: Maybe Box
          -> (Resources -> StockPriceData -> Box -> String)
          -> Maybe String
      filterLabelGroup maybeRelativeBounds textFunction
        | isNothing maybeRelativeBounds = Nothing
        | isMouseWithinBounds = Just text
        | otherwise = Nothing
        where
          absoluteBounds = convertRelativeBounds bounds $
              fromJust maybeRelativeBounds
          isMouseWithinBounds = boxContains absoluteBounds
              (mousePosition resources)
          text = textFunction resources priceData absoluteBounds

      maybeTextItems = map (uncurry filterLabelGroup) labelGroups

  in listToMaybe $ catMaybes maybeTextItems

convertRelativeBounds :: Box -> Box -> Box
convertRelativeBounds bounds relativeBounds =
  let (centerX, centerY) = boxCenter bounds
      Box (relativeLeft, relativeTop)
          (relativeRight, relativeBottom) = relativeBounds
      newLeft = centerX + boxWidth bounds / 2 * relativeLeft
      newRight = centerX + boxWidth bounds / 2 * relativeRight
      newTop = centerY + boxHeight bounds / 2 * relativeTop
      newBottom = centerY + boxHeight bounds / 2 * relativeBottom
  in Box (newLeft, newTop) (newRight, newBottom)

getPriceText :: Resources -> StockPriceData -> Box -> String
getPriceText resources priceData bounds = priceText
  where
    (_, mouseY) = mousePosition resources
    (minPrice, maxPrice) = getPriceRange priceData
    priceRange = realToFrac $ maxPrice - minPrice
    heightPercentage = (realToFrac mouseY - boxBottom bounds) / boxHeight bounds
    price = minPrice + realToFrac (priceRange * heightPercentage)
    priceText = printf "%+.2f" price
 
getVolumeText :: Resources -> StockPriceData -> Box -> String
getVolumeText resources priceData bounds = volumeText
  where
    (_, mouseY) = mousePosition resources
    numElements = numDailyElements priceData
    maxVolume = maximum $ map volume $ take numElements $ prices priceData
    heightPercentage = (realToFrac mouseY - boxBottom bounds) / boxHeight bounds
    volumeValue = floor $ realToFrac maxVolume * heightPercentage

    reduceVolume :: Int -> Int
    reduceVolume volumeValue = volumeValue `div` 1000

    printVolume = printf "%+dK"
    volumeText = printVolume $ reduceVolume volumeValue

getStochasticText :: Resources -> StockPriceData -> Box -> String
getStochasticText resources priceData bounds = stochasticText
  where
    (_, mouseY) = mousePosition resources
    heightPercentage = (realToFrac mouseY - boxBottom bounds) / boxHeight bounds
    stochastic :: Float
    stochastic = realToFrac heightPercentage * 100.0
    stochasticText = printf "%.0f%%" stochastic

renderCrosshair :: Resources -> Box -> Maybe String -> Maybe String -> IO ()
renderCrosshair resources bounds maybeTextX maybeTextY = do
  let (mouseX, mouseY) = mousePosition resources
      (lowAlpha, highAlpha) = (0.1, 1)
      lineColor4 = red4
      textPadding = 5
      textSpec = TextSpec (font resources) 12

  color $ yellow4 1

  when (isJust maybeTextX) $
    preservingMatrix $ do
      let translateX = (-boxWidth bounds / 2) + textPadding
          translateY = (-boxHeight bounds / 2) - boxBottom bounds
              + mouseY + textPadding
      translate $ vector3 translateX translateY 0
      renderText $ textSpec $ fromJust maybeTextX

  when (isJust maybeTextY) $
    preservingMatrix $ do
      let fullTextSpec = textSpec $ fromJust maybeTextY
      textBox <- measureText fullTextSpec
      let translateX = (-boxWidth bounds / 2) - boxLeft bounds
              + mouseX - (boxWidth textBox / 2)
          translateY = boxHeight bounds / 2 - boxHeight textBox - textPadding
      translate $ vector3 translateX translateY 0
      renderText fullTextSpec

  preservingMatrix $ do
    translateToWindowLowerLeft bounds
    renderPrimitive Lines $ do
      -- Vertical line
      color $ lineColor4 lowAlpha 
      vertex $ vertex2 mouseX (boxBottom bounds)

      color $ lineColor4 highAlpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 lowAlpha
      vertex $ vertex2 mouseX (boxTop bounds)

      -- Horizontal line
      color $ lineColor4 lowAlpha 
      vertex $ vertex2 (boxLeft bounds) mouseY

      color $ lineColor4 highAlpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 lowAlpha
      vertex $ vertex2 (boxRight bounds) mouseY

renderPriceInfoBox :: Resources -> Box -> StockPriceData -> IO ()
renderPriceInfoBox resources bounds priceData = do
  let maybePrice = getPriceDataForMousePosition resources bounds priceData
  case maybePrice of
    Just price -> renderPriceText resources bounds price
    _ -> return ()

getVerticalAxisText :: Resources -> Box -> StockPriceData -> Maybe String
getVerticalAxisText resources bounds priceData = do
  let maybePrice = getPriceDataForMousePosition resources bounds priceData
  case maybePrice of
    Just price -> Just $ formatTime defaultTimeLocale "%D" $ endTime price
    _ -> Nothing

getPriceDataForMousePosition :: Resources -> Box -> StockPriceData
    -> Maybe Price
getPriceDataForMousePosition resources bounds priceData
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

renderPriceText :: Resources -> Box -> Price -> IO ()
renderPriceText resources bounds price = do
  let windowPadding = 10
      bubblePadding = 7
      lineSpacing = 3
      alpha = 1

      textSpec = TextSpec (font resources) 12
      openLabel = "Open: "
      closeLabel = "Close: "
      highLabel = "High: "
      lowLabel = "Low: "
      volumeLabel = "Volume: "

      printFloat = printf "%+.2f"
      openText = openLabel ++ (printFloat . open) price
      closeText = closeLabel ++ (printFloat . close) price
      highText = highLabel ++ (printFloat . high) price
      lowText = lowLabel ++ (printFloat . low) price

      printVolume = printf "%+dK"
      reduceVolume volume = volume `div` 1000
      volumeText = volumeLabel ++ (printVolume . reduceVolume . volume) price

  openLabelBox <- measureText $ textSpec openLabel
  closeLabelBox <- measureText $ textSpec closeLabel
  highLabelBox <- measureText $ textSpec highLabel
  lowLabelBox <- measureText $ textSpec lowLabel
  volumeLabelBox <- measureText $ textSpec volumeLabel

  openTextBox <- measureText $ textSpec openText
  closeTextBox <- measureText $ textSpec closeText
  highTextBox <- measureText $ textSpec highText
  lowTextBox <- measureText $ textSpec lowText
  volumeTextBox <- measureText $ textSpec volumeText

  let textLabels = [closeLabel, openLabel, highLabel, lowLabel, volumeLabel]
      textLabelBoxes = [closeLabelBox, openLabelBox, highLabelBox, lowLabelBox,
          volumeLabelBox]

      textItems = [closeText, openText, highText, lowText, volumeText]
      textBoxes = [closeTextBox, openTextBox, highTextBox, lowTextBox,
          volumeTextBox]

      largestLabelWidth = maximum $ map boxWidth textLabelBoxes
      getTextIndentation labelBox = largestLabelWidth - boxWidth labelBox
      textIndents = map getTextIndentation textLabelBoxes
      textWidths = map (\(indent, box) -> indent + boxWidth box)
          (zip textIndents textBoxes)

      largestTextWidth = maximum textWidths
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
    mapM_ (\(textIndent, textBox, text) -> do
        translate $ vector3 textIndent (-boxHeight textBox) 0
        renderText $ textSpec text
        translate $ vector3 (-textIndent) (-lineSpacing) 0
        ) (zip3 textIndents textBoxes textItems)

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0

