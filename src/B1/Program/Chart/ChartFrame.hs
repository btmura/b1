module B1.Program.Chart.ChartFrame
  ( FrameInput(..)
  , FrameOutput(..)
  , FrameState
  , drawChartFrame
  , newFrameState
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
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.Instructions as I

data FrameInput = FrameInput
  { bounds :: Box
  , inputState :: FrameState
  }

data FrameOutput = FrameOutput
  { outputState :: FrameState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol
  }

data FrameState = FrameState
  { currentSymbol :: String
  , nextSymbol :: String
  , currentFrame :: Maybe Frame
  , previousFrame :: Maybe Frame
  }

data Frame = Frame
  { content :: Content
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

data Content = Instructions | Chart Symbol C.ChartState

newFrameState :: FrameState
newFrameState = FrameState
  { currentSymbol = ""
  , nextSymbol = ""
  , currentFrame = Just Frame
    { content = Instructions
    , scaleAnimation = incomingScaleAnimation
    , alphaAnimation = incomingAlphaAnimation
    }
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

drawChartFrame :: Resources -> FrameInput -> IO FrameOutput
drawChartFrame resources frameInput@FrameInput { inputState = state } = do
  midState <- refreshSymbolState resources state

  let revisedFrameInput = frameInput { inputState = midState }
      drawFrameShort = drawFrame resources revisedFrameInput
  maybeNextCurrentFrameState <- drawFrameShort $ currentFrame midState
  maybeNextPreviousFrameState <- drawFrameShort $ previousFrame midState

  drawNextSymbol resources revisedFrameInput 

  let (nextCurrentFrame, isCurrentContentDirty, nextAddedSymbol) =
          maybeNextCurrentFrameState
      (nextPreviousFrame, isPreviousContentDirty, _) =
          maybeNextPreviousFrameState
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
  return FrameOutput
    { outputState = nextState
    , isDirty = nextDirty
    , addedSymbol = nextAddedSymbol
    } 

-- TODO: Check if the Chart's MVar is empty...
isDirtyFrame :: Frame -> Bool
isDirtyFrame (Frame
    { scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) = any (snd . current) [scaleAnimation, alphaAnimation]

drawFrame :: Resources -> FrameInput -> Maybe Frame
    -> IO (Maybe Frame, Dirty, Maybe Symbol)

drawFrame resources _ Nothing = return (Nothing, False, Nothing)

drawFrame resources
    frameInput@FrameInput { bounds = bounds }
    (Just frame@Frame
      { content = content
      , scaleAnimation = scaleAnimation
      , alphaAnimation = alphaAnimation
      }) = 
  preservingMatrix $ do
    translateToCenter bounds
    scale3 scaleAmount scaleAmount 1
    color $ blue alphaAmount
    drawFrameBorder resources frameInput
    (nextContent, isDirty, addedSymbol) <- drawFrameContent resources
        frameInput content alphaAmount
    return (Just frame { content = nextContent }, isDirty, addedSymbol)
  where
    scaleAmount = fst . current $ scaleAnimation
    alphaAmount = fst . current $ alphaAnimation

drawFrameContent :: Resources -> FrameInput -> Content -> GLfloat
    -> IO (Content, Dirty, Maybe Symbol)

drawFrameContent resources FrameInput { bounds = bounds }
    Instructions alpha = do
  output <- I.drawInstructions resources input
  return $ (Instructions, I.isDirty output, Nothing)
  where
    input = I.InstructionsInput
      { I.bounds = bounds
      , I.alpha = alpha
      }

drawFrameContent resources FrameInput { bounds = bounds }
    (Chart symbol state) alpha = do
  output <- C.drawChart resources input
  return $ (Chart symbol (C.outputState output), C.isDirty output,
      C.addedSymbol output)
  where
    input = C.ChartInput
      { C.bounds = boxShrink bounds contentPadding
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
  { scaleAnimation = next $ scaleAnimation frame
  , alphaAnimation = next $ alphaAnimation frame
  }

refreshSymbolState :: Resources -> FrameState -> IO FrameState
refreshSymbolState resources
  | checkKeyPress (SpecialKey BACKSPACE) = handleBackspaceKey 
  | checkKeyPress (SpecialKey ENTER) = handleEnterKey 
  | checkKeyPress (SpecialKey ESC) = handleEscapeKey 
  | isJust maybeLetterKey = handleCharKey $ fromJust maybeLetterKey
  | otherwise = handleNoKey
  where
    checkKeyPress = isKeyPressed resources
    maybeLetterKey = getKeyPressed resources $ map CharKey ['A'..'Z']

-- Append to the next symbol if the key is just a character...
handleCharKey :: Key -> FrameState -> IO FrameState
handleCharKey (CharKey char) state@FrameState { nextSymbol = nextSymbol }
  | isAlpha char = return $ state { nextSymbol = nextSymbol ++ [char] }
  | otherwise = return state
handleCharKey _ state = return state

-- BACKSPACE deletes one character in a symbol...
handleBackspaceKey state@FrameState { nextSymbol = nextSymbol }
  | length nextSymbol < 1 = return state
  | otherwise = return state { nextSymbol = trimmedSymbol }
  where
    trimmedSymbol = take (length nextSymbol - 1) nextSymbol

-- ENTER makes the next symbol the current symbol.
handleEnterKey state@FrameState
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
handleEscapeKey state = return state { nextSymbol = "" }

handleNoKey state = return state

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

contentPadding = 5::GLfloat -- ^ Padding on one side.
cornerRadius = 10::GLfloat
cornerVertices = 5::Int

drawFrameBorder :: Resources -> FrameInput -> IO ()
drawFrameBorder resources FrameInput { bounds = bounds } =
  drawRoundedRectangle width height cornerRadius cornerVertices
  where
     width = boxWidth bounds - contentPadding * 2
     height = boxHeight bounds - contentPadding * 2

drawNextSymbol :: Resources -> FrameInput -> IO ()
drawNextSymbol _
    FrameInput { inputState = FrameState { nextSymbol = "" } } = return ()
drawNextSymbol resources
    FrameInput
      { bounds = bounds
      , inputState = FrameState { nextSymbol = nextSymbol }
      } = do

  boundingBox <- measureText textSpec
  let textBubbleWidth = boxWidth boundingBox + textBubblePadding * 2
      textBubbleHeight = boxHeight boundingBox + textBubblePadding * 2
      (centerX, centerY) = boxCenter boundingBox

  preservingMatrix $ do 
    translateToCenter bounds

    -- Disable blending or else the background won't work.
    blend $= Disabled

    color $ black 1
    fillRoundedRectangle textBubbleWidth textBubbleHeight
        cornerRadius cornerVertices

    color $ blue 1
    drawRoundedRectangle textBubbleWidth textBubbleHeight
        cornerRadius cornerVertices

    -- Renable the blending now...
    blend $= Enabled

    color $ green 1
    translate $ vector3 (-centerX) (-centerY) 0
    renderText textSpec

  where
    textSpec = TextSpec (font resources) 48  nextSymbol
    textBubblePadding = 15

