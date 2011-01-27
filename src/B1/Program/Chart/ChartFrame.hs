module B1.Program.Chart.ChartFrame
  ( drawChartFrame
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Data.Range
import B1.Data.Price.Google
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.Instructions as I

data Content = Instructions | Chart Symbol C.ChartState

data Frame = Frame
  { content :: Content
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

data FrameState = FrameState
  { currentSymbol :: String
  , nextSymbol :: String
  , currentFrame :: Maybe Frame
  , previousFrame :: Maybe Frame
  }

drawChartFrame :: Resources -> IO (Action Resources Dirty, Dirty)
drawChartFrame resources = drawChartFrameLoop initState resources
  where
    instructionsFrame = Frame
      { content = Instructions
      , scaleAnimation = incomingScaleAnimation
      , alphaAnimation = incomingAlphaAnimation
      }

    initState = FrameState
      { currentSymbol = ""
      , nextSymbol = ""
      , currentFrame = Just instructionsFrame 
      , previousFrame = Nothing
      }

incomingScaleAnimation :: Animation (GLfloat, Dirty)
incomingScaleAnimation = animateOnce $ linearRange 1 1 30

incomingAlphaAnimation :: Animation (GLfloat, Dirty)
incomingAlphaAnimation = animateOnce $ linearRange 0 1 30

outgoingScaleAnimation :: Animation (GLfloat, Dirty)
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 30

outgoingAlphaAnimation :: Animation (GLfloat, Dirty)
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 30

drawChartFrameLoop :: FrameState -> Resources
    -> IO (Action Resources Dirty, Dirty)
drawChartFrameLoop state resources = do
  loadIdentity
  translateToCenter resources

  midState <- refreshSymbolState resources state

  let drawFrameShort = drawFrame resources midState
  maybeNextCurrentFrameState <- drawFrameShort $ currentFrame midState
  maybeNextPreviousFrameState <- drawFrameShort $ previousFrame midState

  drawNextSymbol resources midState

  let (nextCurrentFrame, isCurrentContentDirty) = maybeNextCurrentFrameState
      (nextPreviousFrame, isPreviousContentDirty) = maybeNextPreviousFrameState
      nextState = refreshFrameAnimations resources $ midState
        { currentFrame = nextCurrentFrame
        , previousFrame = nextPreviousFrame
        }
      bothFrames = [currentFrame nextState, previousFrame nextState]
      nextDirty = any id
        [ any isDirtyFrame (catMaybes bothFrames)
        , isCurrentContentDirty
        , isPreviousContentDirty
        ]
  return (Action (drawChartFrameLoop nextState), nextDirty)

-- TODO: Check if the Chart's MVar is empty...
isDirtyFrame :: Frame -> Bool
isDirtyFrame (Frame
    { scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) = any (snd . getCurrentFrame) [scaleAnimation, alphaAnimation]

translateToCenter :: Resources -> IO ()
translateToCenter resources =
  translate $ vector3 (sideBarWidth resources + mainFrameWidth resources / 2)
      (mainFrameHeight resources / 2) 0

mainFrameWidth :: Resources -> GLfloat
mainFrameWidth resources = windowWidth resources - sideBarWidth resources

mainFrameHeight :: Resources -> GLfloat
mainFrameHeight = windowHeight

drawFrame :: Resources -> FrameState -> Maybe Frame
    -> IO (Maybe Frame, Dirty)

drawFrame resources state Nothing = return (Nothing, False)

drawFrame resources state (Just frame@(Frame
    { content = content
    , scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    })) = 
  preservingMatrix $ do
    scale3 scaleAmount scaleAmount 1
    color $ blue alphaAmount
    drawFrameBorder resources
    (nextContent, isDirty) <- drawFrameContent resources content alphaAmount
    return (Just frame { content = nextContent }, isDirty)
  where
    scaleAmount = fst . getCurrentFrame $ scaleAnimation
    alphaAmount = fst . getCurrentFrame $ alphaAnimation

drawFrameContent :: Resources -> Content -> GLfloat -> IO (Content, Dirty)

drawFrameContent resources Instructions alpha = do
  output <- I.drawInstructions resources input
  return $ (Instructions, I.isDirty output)
  where
    input = I.InstructionsInput
      { I.width = mainFrameWidth resources
      , I.alpha = alpha
      }

drawFrameContent resources (Chart symbol state) alpha = do
  output <- C.drawChart resources input
  return $ (Chart symbol (C.outputState output), C.isDirty output)
  where
    input = C.ChartInput
      { C.width = mainFrameWidth resources - contentPadding
      , C.height = mainFrameHeight resources - contentPadding
      , C.alpha = alpha
      , C.symbol = symbol
      , C.inputState = state
      }

refreshFrameAnimations :: Resources -> FrameState -> FrameState
refreshFrameAnimations resources
    state@FrameState
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

refreshSymbolState :: Resources -> FrameState -> IO FrameState

-- Append to the next symbol if the key is just a character...
refreshSymbolState (Resources { keyPress = Just (CharKey char) })
    state@FrameState { nextSymbol = nextSymbol }
  | isAlpha char = return $ state { nextSymbol = nextSymbol ++ [char] }
  | otherwise = return state

-- BACKSPACE deletes one character in a symbol...
refreshSymbolState (Resources { keyPress = Just (SpecialKey BACKSPACE) })
    state@FrameState { nextSymbol = nextSymbol }
  | length nextSymbol < 1 = return state
  | otherwise = return state { nextSymbol = trimmedSymbol }
  where
    trimmedSymbol = take (length nextSymbol - 1) nextSymbol

-- ENTER makes the next symbol the current symbol.
refreshSymbolState (Resources { keyPress = Just (SpecialKey ENTER) })
    state@FrameState
      { nextSymbol = nextSymbol
      , currentFrame = currentFrame
      }
  | nextSymbol == "" = return state
  | otherwise = do
    chartContent <- newChartContent nextSymbol
    return state
      { currentSymbol = nextSymbol
      , nextSymbol = ""
      , currentFrame = newCurrentFrame chartContent
      , previousFrame = newPreviousFrame currentFrame
      }

-- ESC cancels the next symbol.
refreshSymbolState (Resources { keyPress = Just (SpecialKey ESC) })
    state = return state { nextSymbol = "" }

-- Drop all other events.
refreshSymbolState _ state = return state

newChartContent :: Symbol -> IO Content
newChartContent symbol = do
  state <- C.newChartState symbol
  return $ Chart symbol state

newCurrentFrame :: Content -> Maybe Frame
newCurrentFrame content = Just Frame
  { content = content
  , scaleAnimation = incomingScaleAnimation
  , alphaAnimation = incomingAlphaAnimation
  }

newPreviousFrame :: Maybe Frame -> Maybe Frame
newPreviousFrame Nothing = Nothing
newPreviousFrame (Just frame) = Just $ frame 
  { scaleAnimation = outgoingScaleAnimation
  , alphaAnimation = outgoingAlphaAnimation
  }

contentPadding = 10::GLfloat
cornerRadius = 10::GLfloat
cornerVertices = 5::Int

drawFrameBorder :: Resources -> IO ()
drawFrameBorder resources =
  drawRoundedRectangle width height cornerRadius cornerVertices
  where
     width = mainFrameWidth resources - contentPadding
     height = mainFrameHeight resources - contentPadding

drawNextSymbol :: Resources -> FrameState -> IO ()
drawNextSymbol _ FrameState { nextSymbol = "" } = return ()
drawNextSymbol resources
    FrameState { nextSymbol = nextSymbol } = do
  [left, bottom, right, top] <- prepareLayoutText resources fontSize
      layoutLineLength nextSymbol

  let textWidth = abs $ right - left
      textHeight = abs $ bottom - top

      textBubblePadding = 15
      textBubbleWidth = textWidth + textBubblePadding*2
      textBubbleHeight = textHeight + textBubblePadding*2

      textCenterX = -(left + textWidth / 2)
      textCenterY = -(top - textHeight / 2)

  -- Disable blending or else the background won't work.
  blend $= Disabled

  preservingMatrix $ do 
    color $ black 1
    fillRoundedRectangle textBubbleWidth textBubbleHeight
        cornerRadius cornerVertices

    color $ blue 1
    drawRoundedRectangle textBubbleWidth textBubbleHeight
        cornerRadius cornerVertices

    color $ green 1
    translate $ vector3 textCenterX textCenterY 0
    renderLayoutText resources nextSymbol

  blend $= Enabled

  where
    fontSize = 48
    layoutLineLength = realToFrac $ mainFrameWidth resources - contentPadding



