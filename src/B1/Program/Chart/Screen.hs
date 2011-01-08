module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Control.Monad
import Data.Char
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = 
  return (Action (drawScreenLoop drawSideBar
      (drawMainChart (gradualRange 0 1 100) ("", ""))), True)

drawScreenLoop :: (Resources -> IO (Action Resources Dirty, Dirty))
    -> (Resources -> IO (Action Resources Dirty, Dirty))
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarAction mainChartAction input = do
  (Action nextSideBarAction, sideBarDirty) <- sideBarAction input
  (Action nextMainChartAction, mainChartDirty) <- mainChartAction input
  return (Action (drawScreenLoop nextSideBarAction nextMainChartAction),
      sideBarDirty || mainChartDirty)

sideBarWidth = 175

drawSideBar :: Resources -> IO (Action Resources Dirty, Dirty)
drawSideBar resources = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth / 2) (sideBarHeight / 2) 1
  color $ color3 0 0 1
  drawSquarePlaceholder
  return (Action drawSideBar, False)

drawMainChart :: [GLfloat] -> (String, String) -> Resources
    -> IO (Action Resources Dirty, Dirty)
drawMainChart rangeValues@(rangeValue:nextRangeValues) inputSymbols
    resources = do
  loadIdentity
  translate $ vector3 (sideBarWidth + mainChartWidth resources / 2)
      (mainChartHeight resources / 2) 0

  scale3 rangeValue 1 1

  color $ color4 0.25 1 0 rangeValue
  let nextInputSymbols@(currentSymbol, nextSymbol) =
          getInputSymbols inputSymbols resources
  if nextSymbol == ""
    then drawCenteredInstructions resources
    else drawNextSymbol nextSymbol resources

  color $ color4 0 0.25 1 rangeValue
  drawChart resources

  case nextRangeValues of
    [] -> return (Action (drawMainChart rangeValues nextInputSymbols), False)
    _ -> return (Action (drawMainChart nextRangeValues nextInputSymbols), True)

mainChartWidth :: Resources -> GLfloat
mainChartWidth resources = realToFrac (windowWidth resources) - sideBarWidth

mainChartHeight :: Resources -> GLfloat
mainChartHeight resources = realToFrac (windowHeight resources)

getInputSymbols :: (String, String) -> Resources -> (String, String)

-- Append to the next symbol if the key is just a character...
getInputSymbols inputs@(currentSymbol, nextSymbol) 
    (Resources { keyPress = Just (CharKey char) })
  | isAlpha char = (currentSymbol, nextSymbol ++ [char])
  | otherwise = inputs

-- Handle special keys. 
getInputSymbols inputs@(currentSymbol, nextSymbol)
    (Resources { keyPress = Just (SpecialKey special) }) =
  case special of
    ENTER -> (nextSymbol, "")
    ESC -> (currentSymbol, "")
    _ -> inputs

-- Drop all other events.
getInputSymbols inputs _ = inputs

chartPadding :: GLfloat
chartPadding = 10

drawChart :: Resources -> IO ()
drawChart resources = 
  preservingMatrix $ do
    drawRoundedRectangle (mainChartWidth resources - chartPadding)
        (mainChartHeight resources - chartPadding) cornerRadius cornerVertices
  where
    cornerRadius = 15
    cornerVertices = 5

drawCenteredInstructions :: Resources -> IO ()
drawCenteredInstructions resources = do
  layout <- createSimpleLayout
  setFontFaceSize (font resources) 18 72
  setLayoutFont layout (font resources)
  setLayoutLineLength layout 
      (realToFrac (mainChartWidth resources - chartPadding))

  let instructions = "Type in symbol and press ENTER..."
  [left, bottom, _, right, top, _] <- getLayoutBBox layout instructions

  let centerX = -(left + (abs (right - left)) / 2)
      centerY = -(top - (abs (bottom - top)) / 2)
  preservingMatrix $ do 
    translate $ vector3 centerX centerY 0
    renderLayout layout instructions

  destroyLayout layout

drawNextSymbol :: String -> Resources -> IO ()
drawNextSymbol nextSymbol resources = do
  layout <- createSimpleLayout
  setFontFaceSize (font resources) 48 72
  setLayoutFont layout (font resources)
  setLayoutLineLength layout 
      (realToFrac (mainChartWidth resources - chartPadding))

  [left, bottom, _, right, top, _] <- getLayoutBBox layout nextSymbol

  let centerX = -(left + (abs (right - left)) / 2)
      centerY = -(top - (abs (bottom - top)) / 2)
  preservingMatrix $ do 
    translate $ vector3 centerX centerY 0
    renderLayout layout nextSymbol

  destroyLayout layout

