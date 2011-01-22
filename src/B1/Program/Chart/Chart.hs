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

type Symbol = String

data Chart = Instructions | Chart Symbol

data Frame = Frame
  { chart :: Chart
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

data ChartState = ChartState
  { currentSymbol :: String
  , nextSymbol :: String
  , currentFrame :: Frame
  , previousFrame :: Maybe Frame
  }

drawChart :: Resources -> IO (Action Resources Dirty, Dirty)
drawChart resources = drawChartLoop initState resources
  where
    currentFrame = Frame
      { chart = Instructions
      , scaleAnimation = animateOnce $ linearRange 0.5 1 30
      , alphaAnimation = animateOnce $ linearRange 0 1 30
      }

    initState = ChartState
      { currentSymbol = ""
      , nextSymbol = ""
      , currentFrame = currentFrame 
      , previousFrame = Nothing
      }

drawChartLoop :: ChartState -> Resources -> IO (Action Resources Dirty, Dirty)
drawChartLoop state@ChartState
    { currentFrame = currentFrame
    } resources = do

  loadIdentity
  translateToCenter resources

  drawCurrentFrame currentFrame resources

  return (Action (drawChartLoop nextState), True)

  where
    nextCurrentFrame = currentFrame
      { scaleAnimation = getNextAnimation $ scaleAnimation currentFrame
      , alphaAnimation = getNextAnimation $ alphaAnimation currentFrame
      }

    nextState = state
      { currentFrame = nextCurrentFrame
      }

translateToCenter :: Resources -> IO ()
translateToCenter resources =
  translate $ vector3 (sideBarWidth resources + (mainChartWidth resources) / 2)
      (mainChartHeight resources / 2) 0

drawCurrentFrame :: Frame -> Resources -> IO ()
drawCurrentFrame (Frame
    { chart = Instructions
    , scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) resources = do
  let scaleAmount = fst . getCurrentFrame $ scaleAnimation
      alphaAmount = fst . getCurrentFrame $ alphaAnimation
  preservingMatrix $ do
    scale3 scaleAmount scaleAmount 1
    color $ color4 0.25 1 0 alphaAmount
    drawCenteredInstructions resources
    color $ color4 0 0.25 1 alphaAmount
    drawChartBorder resources

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



