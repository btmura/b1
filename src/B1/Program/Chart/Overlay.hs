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
import Data.Time
import Data.Time.Format
import Graphics.Rendering.OpenGL
import System.Locale
import Text.Printf

import B1.Data.Direction
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
  , infoHorizontalPosition :: Direction
  , infoVerticalPosition :: Direction
  , minInfoBubbleWidth :: GLfloat
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

textPadding :: GLfloat
textPadding = 5

lineColor4 :: GLfloat -> Color4 GLfloat
lineColor4 = gray4

newOverlayState :: OverlayOptions -> StockData -> OverlayState
newOverlayState options stockData = OverlayState
  { options = options
  , stockData = stockData
  , infoHorizontalPosition = West
  , infoVerticalPosition = East
  , minInfoBubbleWidth = 0
  }

drawOverlay :: Resources -> OverlayInput -> IO OverlayOutput
drawOverlay resources
    input@OverlayInput
      { inputState = inputState@OverlayState
        { stockData = stockData
        }
      } = do
  outputState <- handleStockData (renderOverlay resources input)
      (\_ -> return inputState) inputState stockData
  return OverlayOutput
    { outputState = outputState
    , isDirty = False
    }

renderOverlay :: Resources -> OverlayInput -> StockPriceData -> IO OverlayState
renderOverlay resources
    input@OverlayInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@OverlayState
        { options = OverlayOptions
          { boundSet = boundSet
          }
        , infoHorizontalPosition = horizontalPosition
        , infoVerticalPosition = verticalPosition
        , minInfoBubbleWidth = minInfoBubbleWidth
        }
      }
    priceData =
  if alpha >= 1 -- Too expensive to draw the overlay while animating
      && not (isMouseDrag resources)
      && boxContains bounds (mousePosition resources)
    then do
      let maybeTextX = getHorizontalAxisText resources bounds boundSet priceData
          maybeTextY = getVerticalAxisText resources bounds priceData
          maybePrice = getPriceDataForMousePosition resources bounds priceData
      case maybePrice of
        Just price -> do
          (horizontalDirection, verticalDirection, bubbleWidth)
              <- renderPriceInfoBox resources bounds boundSet
                  horizontalPosition verticalPosition
                  minInfoBubbleWidth  price
          renderCrosshair resources bounds maybeTextY
          renderHorizontalAxisText resources bounds maybeTextX
              horizontalDirection
          renderVerticalAxisText resources bounds maybeTextY
          return state
            { infoHorizontalPosition = horizontalDirection
            , infoVerticalPosition = verticalDirection
            , minInfoBubbleWidth = bubbleWidth
            }
        _ -> return state
    else
      return state

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

renderCrosshair :: Resources -> Box -> Maybe String -> IO ()
renderCrosshair resources bounds maybeTextY = do
  let (mouseX, mouseY) = mousePosition resources
      (lowAlpha, highAlpha) = (0.1, 1)
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

renderHorizontalAxisText :: Resources -> Box -> Maybe String -> Direction
    -> IO ()
renderHorizontalAxisText resources bounds maybeText direction =
  case maybeText of
    Just text -> do
      let textSpec = TextSpec (font resources) 14 text
      textBox <- measureText textSpec

      let windowPadding = -2
          bubblePadding = 5
          bubbleWidth = bubblePadding + boxWidth textBox + bubblePadding
          bubbleHeight = bubblePadding + boxHeight textBox + bubblePadding

          (mouseX, mouseY) = mousePosition resources
          bubbleX = case direction of
                      West -> boxLeft bounds + bubbleWidth / 2 + windowPadding
                      _ -> boxRight bounds - bubbleWidth / 2 - windowPadding
          bubbleY = mouseY
          (bubbleTranslateX, bubbleTranslateY) =
              globalTranslate bounds (bubbleX, bubbleY)

      preservingMatrix $ do
        translate $ vector3 bubbleTranslateX bubbleTranslateY 0
        opaqueBubble bubbleWidth bubbleHeight bubblePadding
            (black4 1) (lineColor4 1)
       
      let textX = case direction of
                    West -> boxLeft bounds + bubblePadding + windowPadding
                    _ -> boxRight bounds - bubblePadding - boxWidth textBox
                        - windowPadding
          textY = mouseY - boxHeight textBox / 2
          (textTranslateX, textTranslateY) =
              globalTranslate bounds (textX, textY)

      preservingMatrix $ do
        translate $ vector3 textTranslateX textTranslateY 0
        color $ yellow4 1
        renderText textSpec

    _ -> return ()

-- TODO: Put into a separate module
globalTranslate :: Box -> Point -> Point
globalTranslate bounds (globalX, globalY) = 
  let translateX = -boxWidth bounds / 2 - boxLeft bounds + globalX
      translateY = -boxHeight bounds / 2 - boxBottom bounds + globalY
  in (translateX, translateY)

renderVerticalAxisText :: Resources -> Box -> Maybe String -> IO ()
renderVerticalAxisText resources bounds maybeText =
  case maybeText of
    Just text -> do
      let textSpec = TextSpec (font resources) 14 text
      textBox <- measureText textSpec

      let windowPadding = -2
          bubblePadding = 5
          bubbleWidth = bubblePadding + boxWidth textBox + bubblePadding
          bubbleHeight = bubblePadding + boxHeight textBox + bubblePadding

          (mouseX, mouseY) = mousePosition resources
          centerX 
            | mouseX - bubbleWidth / 2 < boxLeft bounds =
                  boxLeft bounds + bubbleWidth / 2
            | mouseX + bubbleWidth / 2 > boxRight bounds =
                  boxRight bounds - bubbleWidth / 2
            | otherwise = mouseX

          bubbleX = centerX
          bubbleY = boxTop bounds - bubbleHeight / 2 - windowPadding
          (bubbleTranslateX, bubbleTranslateY) =
              globalTranslate bounds (bubbleX, bubbleY)

      preservingMatrix $ do
        translate $ vector3 bubbleTranslateX bubbleTranslateY 0
        opaqueBubble bubbleWidth bubbleHeight bubblePadding
            (black4 1) (lineColor4 1)

      let textX = centerX - boxWidth textBox / 2
          textY = boxTop bounds - boxHeight textBox - bubblePadding
              - windowPadding
          (textTranslateX, textTranslateY) =
              globalTranslate bounds (textX, textY)
       
      preservingMatrix $ do
        translate $ vector3 textTranslateX textTranslateY 0
        color $ yellow4 1
        renderText textSpec

    _ -> return ()

getVerticalAxisText :: Resources -> Box -> StockPriceData -> Maybe String
getVerticalAxisText resources bounds priceData = do
  let maybePrice = getPriceDataForMousePosition resources bounds priceData
  case maybePrice of
    Just price -> Just $ formatTime Data.Time.defaultTimeLocale "%D" $ endTime price
    _ -> Nothing

getPriceDataForMousePosition :: Resources -> Box -> StockPriceData
    -> Maybe Price
getPriceDataForMousePosition resources bounds priceData
  | mouseX < left = Nothing
  | mouseX >= right = Nothing
  | index < numElements = Just price
  | otherwise = Nothing
  where
    (mouseX, _) = mousePosition resources
    Box (left, _) (right, _) = bounds
    numElements = numDailyElements priceData
    elementWidth = boxWidth bounds / realToFrac numElements
    index = floor $ (mouseX - left) / elementWidth
    reverseIndex = numElements - 1 - index
    price = prices priceData !! reverseIndex

renderPriceInfoBox :: Resources -> Box -> OverlayBoundSet
    -> Direction -> Direction -> GLfloat -> Price
    -> IO (Direction, Direction, GLfloat)
renderPriceInfoBox resources bounds boundSet
    horizontalPosition verticalPosition
    minInfoBubbleWidth price = do
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
      tightBubbleWidth = bubblePadding + largestTextWidth + bubblePadding
      bubbleWidth = max minInfoBubbleWidth tightBubbleWidth

      totalTextHeight = sum $ map ((+) lineSpacing . boxHeight) textBoxes
      bubbleHeight = bubblePadding + totalTextHeight + bubblePadding

      subBounds = case graphBounds boundSet of
                    Just sub -> convertRelativeBounds bounds sub
                    _ -> bounds
      (centerX, centerY) = boxCenter subBounds

      (mouseX, mouseY) = mousePosition resources
      leftBubbleLeft = boxLeft subBounds + windowPadding
      leftBubbleRight = leftBubbleLeft + bubbleWidth
      rightBubbleLeft = boxRight subBounds - windowPadding - bubbleWidth
      rightBubbleRight = rightBubbleLeft + bubbleWidth
      (bubbleX, horizontalDirection)
        | mouseX < leftBubbleRight = (rightBubbleLeft, East)
        | mouseX > rightBubbleLeft = (leftBubbleLeft, West)
        | otherwise = case horizontalPosition of
                        West -> (leftBubbleLeft, West)
                        _ -> (rightBubbleLeft, East)

      topBubbleTop = boxTop subBounds - windowPadding
      topBubbleBottom = topBubbleTop - bubbleHeight

      bottomBubbleBottom = boxBottom subBounds + windowPadding
      bottomBubbleTop = bottomBubbleBottom + bubbleHeight
      (bubbleY, verticalDirection)
        | mouseY > topBubbleBottom = (bottomBubbleTop, South)
        | mouseY < bottomBubbleTop = (topBubbleTop, North)
        | otherwise = case verticalPosition of
                        North -> (topBubbleTop, North)
                        _ -> (bottomBubbleTop, South)

      (translateX, translateY) = globalTranslate bounds (bubbleX, bubbleY)

  preservingMatrix $ do
    translate $ vector3 translateX translateY 0

    preservingMatrix $ do
      translate $ vector3 (bubbleWidth / 2) (-bubbleHeight / 2) 0
      opaqueBubble bubbleWidth bubbleHeight bubblePadding
          (black4 1) (lineColor4 1)

    color $ yellow4 alpha
    translate $ vector3 bubblePadding (-bubblePadding) 0
    mapM_ (\(textIndent, textBox, text) -> do
        translate $ vector3 textIndent (-boxHeight textBox) 0
        renderText $ textSpec text
        translate $ vector3 (-textIndent) (-lineSpacing) 0
        ) (zip3 textIndents textBoxes textItems)

    return (horizontalDirection, verticalDirection, bubbleWidth)

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0

