module B1.Program.Chart.Chart
  ( drawChart
  ) where
  
import Control.Monad
import Data.Char
import Data.Maybe
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
  , currentFrame :: Maybe Frame
  , previousFrame :: Maybe Frame
  }

drawChart :: Resources -> IO (Action Resources Dirty, Dirty)
drawChart resources = drawChartLoop initState resources
  where
    instructionsFrame = Frame
      { chart = Instructions
      , scaleAnimation = incomingScaleAnimation
      , alphaAnimation = incomingAlphaAnimation
      }

    initState = ChartState
      { currentSymbol = ""
      , nextSymbol = ""
      , currentFrame = Just instructionsFrame 
      , previousFrame = Nothing
      }

incomingScaleAnimation :: Animation (GLfloat, Dirty)
incomingScaleAnimation = animateOnce [1]

incomingAlphaAnimation :: Animation (GLfloat, Dirty)
incomingAlphaAnimation = animateOnce $ linearRange 0 1 30

outgoingScaleAnimation :: Animation (GLfloat, Dirty)
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 30

outgoingAlphaAnimation :: Animation (GLfloat, Dirty)
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 30

drawChartLoop :: ChartState -> Resources -> IO (Action Resources Dirty, Dirty)
drawChartLoop state resources = do

  loadIdentity
  translateToCenter resources

  mapM_ (drawFrame resources nextState) allFrames
  drawNextSymbol resources nextState

  return (Action (drawChartLoop nextState), nextDirty)

  where
    nextState = (refreshSymbolState resources
        . refreshCurrentFrame resources) state
    allFrames = catMaybes [currentFrame nextState, previousFrame nextState]
    nextDirty = any isDirtyFrame allFrames

isDirtyFrame :: Frame -> Bool
isDirtyFrame (Frame
    { scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) = any (snd . getCurrentFrame) [scaleAnimation, alphaAnimation]

translateToCenter :: Resources -> IO ()
translateToCenter resources =
  translate $ vector3 (sideBarWidth resources + (mainChartWidth resources) / 2)
      (mainChartHeight resources / 2) 0

mainChartWidth :: Resources -> GLfloat
mainChartWidth resources = windowWidth resources - sideBarWidth resources

mainChartHeight :: Resources -> GLfloat
mainChartHeight resources = windowHeight resources

drawFrame :: Resources -> ChartState -> Frame -> IO ()
drawFrame resources state (Frame
    { chart = chart
    , scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) = do
  let scaleAmount = fst . getCurrentFrame $ scaleAnimation
      alphaAmount = fst . getCurrentFrame $ alphaAnimation
  preservingMatrix $ do
    scale3 scaleAmount scaleAmount 1
    color $ blue alphaAmount
    drawChartBorder resources

    color $ green alphaAmount
    case chart of
      Chart _ -> drawCurrentSymbol resources chart
      _ -> drawCenteredInstructions resources

blue :: GLfloat -> Color4 GLfloat
blue alpha = color4 0 0.25 1 alpha

green :: GLfloat -> Color4 GLfloat
green alpha = color4 0.25 1 0 alpha

black :: GLfloat -> Color4 GLfloat
black alpha = color4 0 0 0 alpha

refreshCurrentFrame :: Resources -> ChartState -> ChartState
refreshCurrentFrame resources
    state@ChartState
      { currentFrame = currentFrame
      , previousFrame = previousFrame
      } = state
  { currentFrame = nextFrame currentFrame
  , previousFrame = nextFrame previousFrame
  }

nextFrame :: Maybe Frame -> Maybe Frame
nextFrame Nothing = Nothing
nextFrame (Just frame) = Just $ frame
  { scaleAnimation = getNextAnimation $ scaleAnimation frame
  , alphaAnimation = getNextAnimation $ alphaAnimation frame
  }

refreshSymbolState :: Resources -> ChartState -> ChartState

-- Append to the next symbol if the key is just a character...
refreshSymbolState (Resources { keyPress = Just (CharKey char) })
    state@ChartState { nextSymbol = nextSymbol }
  | isAlpha char = state { nextSymbol = nextSymbol ++ [char] }
  | otherwise = state

-- ENTER makes the next symbol the current symbol.
refreshSymbolState (Resources { keyPress = Just (SpecialKey ENTER) })
    state@ChartState
      { nextSymbol = nextSymbol
      , currentFrame = currentFrame
      }
  | nextSymbol == "" = state
  | otherwise = state
    { currentSymbol = nextSymbol
    , nextSymbol = ""
    , currentFrame = newCurrentFrame (Chart nextSymbol)
    , previousFrame = newPreviousFrame currentFrame
    }

-- ESC cancels the next symbol.
refreshSymbolState (Resources { keyPress = Just (SpecialKey ESC) })
    state = state { nextSymbol = "" }

-- Drop all other events.
refreshSymbolState _ state = state

newCurrentFrame :: Chart -> Maybe Frame
newCurrentFrame chart = Just $ Frame
  { chart = chart
  , scaleAnimation = incomingScaleAnimation
  , alphaAnimation = incomingAlphaAnimation
  }

newPreviousFrame :: Maybe Frame -> Maybe Frame
newPreviousFrame Nothing = Nothing
newPreviousFrame (Just frame) = Just $ frame 
  { scaleAnimation = outgoingScaleAnimation
  , alphaAnimation = outgoingAlphaAnimation
  }

chartPadding = 10::GLfloat
cornerRadius = 10::GLfloat
cornerVertices = 5::Int

drawChartBorder :: Resources -> IO ()
drawChartBorder resources = do
  let width = mainChartWidth resources - chartPadding
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

drawCurrentSymbol :: Resources -> Chart -> IO ()
drawCurrentSymbol resources (Chart symbol) = do
  layout <- createSimpleLayout
  setFontFaceSize (font resources) 18 72
  setLayoutFont layout (font resources)
  setLayoutLineLength layout
      (realToFrac (mainChartWidth resources - chartPadding))

  [left, bottom, _, right, top, _] <- getLayoutBBox layout symbol

  let symbolPadding = 15
      centerX = -mainChartWidth resources / 2 + symbolPadding
      centerY = mainChartHeight resources / 2 - symbolPadding
          - abs (bottom - top)
  preservingMatrix $ do 
    translate $ vector3 centerX centerY 0
    renderLayout layout symbol

  destroyLayout layout

drawNextSymbol :: Resources -> ChartState -> IO ()
drawNextSymbol _ (ChartState { nextSymbol = "" }) = return ()
drawNextSymbol resources (ChartState { nextSymbol = nextSymbol }) = do
  layout <- createSimpleLayout
  setFontFaceSize (font resources) 48 72
  setLayoutFont layout (font resources)
  setLayoutLineLength layout 
      (realToFrac (mainChartWidth resources - chartPadding))

  [left, bottom, _, right, top, _] <- getLayoutBBox layout nextSymbol

  let width = abs $ right - left
      height = abs $ bottom - top
      padding = 15
      centerX = -(left + (abs (right - left)) / 2)
      centerY = -(top - (abs (bottom - top)) / 2)
  blend $= Disabled
  preservingMatrix $ do 
    color $ black 1
    fillRoundedRectangle (width + padding * 2) (height + padding * 2)
        cornerRadius cornerVertices

    color $ blue 1
    drawRoundedRectangle (width + padding * 2) (height + padding * 2)
        cornerRadius cornerVertices

    color $ green 1
    translate $ vector3 centerX centerY 0
    renderLayout layout nextSymbol
  blend $= Enabled

  destroyLayout layout

