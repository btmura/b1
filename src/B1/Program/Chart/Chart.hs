module B1.Program.Chart.Chart
  ( drawChart
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
import B1.Program.Chart.Animation
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data ChartState = ChartState
  { currentSymbol :: String
  , nextSymbol :: String
  , entryScaleAnimation :: Animation (GLfloat, Dirty)
  , entryAlphaAnimation :: Animation (GLfloat, Dirty)
  } deriving (Show)

drawChart :: Resources -> IO (Action Resources Dirty, Dirty)
drawChart resources = drawChartLoop initState resources
  where
    initState = ChartState
      { currentSymbol = ""
      , nextSymbol = ""
      , entryScaleAnimation = animateOnce $ linearRange 0.5 1 50
      , entryAlphaAnimation = animateOnce $ linearRange 0 1 50
      }

drawChartLoop :: ChartState -> Resources -> IO (Action Resources Dirty, Dirty)
drawChartLoop state resources = do
  loadIdentity
  translate $ vector3 (sideBarWidth resources + (mainChartWidth resources) / 2)
      (mainChartHeight resources / 2) 0

  let scaleAmount = fst . getCurrentFrame . entryScaleAnimation $ state
  scale3 scaleAmount scaleAmount 1

  let alphaAmount = fst . getCurrentFrame . entryAlphaAnimation $ state
  color $ color4 0.25 1 0 alphaAmount

  let newState = refreshSymbolState state resources

  when (currentSymbol newState /= "") $
    drawCurrentSymbol (currentSymbol newState) resources

  when (currentSymbol newState == "" && nextSymbol newState == "") $
    drawCenteredInstructions resources

  when (nextSymbol newState /= "") $
    drawNextSymbol newState resources

  color $ color4 0 0.25 1 alphaAmount
  drawChartBorder resources

  return (Action (drawChartLoop (refreshAnimationState newState)),
      isDirty newState)

mainChartWidth :: Resources -> GLfloat
mainChartWidth resources = windowWidth resources - sideBarWidth resources

mainChartHeight :: Resources -> GLfloat
mainChartHeight resources = windowHeight resources

refreshSymbolState :: ChartState -> Resources -> ChartState

-- Append to the next symbol if the key is just a character...
refreshSymbolState state@ChartState { nextSymbol = nextSymbol }
    (Resources { keyPress = Just (CharKey char) })
  | isAlpha char = state { nextSymbol = nextSymbol ++ [char] }
  | otherwise = state

-- ENTER makes the next symbol the current symbol.
refreshSymbolState state@ChartState { nextSymbol = nextSymbol }
    (Resources { keyPress = Just (SpecialKey ENTER) }) = state
  { currentSymbol = nextSymbol
  , nextSymbol = ""
  }

-- ESC cancels the next symbol.
refreshSymbolState state (Resources { keyPress = Just (SpecialKey ESC) }) =
  state { nextSymbol = "" }

-- Drop all other events.
refreshSymbolState state _ = state

chartPadding :: GLfloat
chartPadding = 10

cornerRadius :: GLfloat
cornerRadius = 15

drawChartBorder :: Resources -> IO ()
drawChartBorder resources = do
  let cornerVertices = 5
      width = mainChartWidth resources - chartPadding
      height = mainChartHeight resources - chartPadding
  preservingMatrix $
    drawRoundedRectangle width height cornerRadius cornerVertices

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

drawCurrentSymbol :: String -> Resources -> IO ()
drawCurrentSymbol symbol resources = do
  layout <- createSimpleLayout
  setFontFaceSize (font resources) 18 72
  setLayoutFont layout (font resources)
  setLayoutLineLength layout
      (realToFrac (mainChartWidth resources - chartPadding))

  [left, bottom, _, right, top, _] <- getLayoutBBox layout symbol

  let symbolPadding = cornerRadius
      centerX = -mainChartWidth resources / 2 + symbolPadding
      centerY = mainChartHeight resources / 2 - symbolPadding
          - abs (bottom - top)
  preservingMatrix $ do 
    translate $ vector3 centerX centerY 0
    renderLayout layout symbol

  destroyLayout layout

drawNextSymbol :: ChartState -> Resources -> IO ()
drawNextSymbol (ChartState { nextSymbol = nextSymbol })
    resources = do

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

refreshAnimationState :: ChartState -> ChartState
refreshAnimationState state@ChartState
    { entryScaleAnimation = entryScaleAnimation
    , entryAlphaAnimation = entryAlphaAnimation
    } = state
    { entryScaleAnimation = getNextAnimation entryScaleAnimation
    , entryAlphaAnimation = getNextAnimation entryAlphaAnimation
    }

isDirty :: ChartState -> Bool
isDirty (ChartState
    { entryScaleAnimation = entryScaleAnimation
    , entryAlphaAnimation = entryAlphaAnimation
    }) = any (snd . getCurrentFrame) allAnimations
  where
    allAnimations = [entryScaleAnimation, entryAlphaAnimation]


