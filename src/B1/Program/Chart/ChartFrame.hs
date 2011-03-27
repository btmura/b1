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
  , symbolRequest :: Maybe Symbol
  , inputState :: FrameState
  }

data FrameOutput = FrameOutput
  { outputState :: FrameState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol
  , selectedSymbol :: Maybe Symbol
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
drawChartFrame resources frameInput@FrameInput { bounds = bounds } = do
  drawNextSymbol resources bounds
      =<< drawFrames resources bounds
      =<< refreshSymbolState resources frameInput

refreshSymbolState :: Resources -> FrameInput -> IO FrameOutput

refreshSymbolState _
    FrameInput
      { symbolRequest = Just symbol
      , inputState = state
      } = 
  loadNextSymbol $ state { nextSymbol = symbol }

refreshSymbolState resources
    FrameInput
      { symbolRequest = Nothing
      , inputState = state
      }
  | checkKeyPress (SpecialKey BACKSPACE) = handleBackspaceKey state
  | checkKeyPress (SpecialKey ENTER) = handleEnterKey state
  | checkKeyPress (SpecialKey ESC) = handleEscapeKey state
  | isJust maybeLetterKey = handleCharKey (fromJust maybeLetterKey) state
  | otherwise = handleNoKey state
  where
    checkKeyPress = isKeyPressed resources
    maybeLetterKey = getKeyPressed resources $ map CharKey ['A'..'Z']

-- BACKSPACE deletes one character in a symbol...
handleBackspaceKey :: FrameState -> IO FrameOutput
handleBackspaceKey state@FrameState { nextSymbol = nextSymbol } =
  return $ FrameOutput
    { outputState = outputState
    , isDirty = isDirty
    , addedSymbol = Nothing
    , selectedSymbol = Nothing
    }
  where
    isNextSymbolEmpty = length nextSymbol < 1
    trimmedSymbol = take (length nextSymbol - 1) nextSymbol
    (outputState, isDirty) =
        if isNextSymbolEmpty
          then (state, False)
          else (state { nextSymbol = trimmedSymbol }, True)

-- ENTER makes the next symbol the current symbol.
handleEnterKey :: FrameState -> IO FrameOutput
handleEnterKey state@FrameState { nextSymbol = nextSymbol } = 
  if nextSymbol == ""
    then return $ FrameOutput
      { outputState = state
      , isDirty = False
      , addedSymbol = Nothing
      , selectedSymbol = Nothing
      }
    else loadNextSymbol state

loadNextSymbol :: FrameState -> IO FrameOutput
loadNextSymbol state@FrameState
    { nextSymbol = nextSymbol
    , currentFrame = currentFrame
    } = do
  chartContent <- newChartContent nextSymbol
  let outputState = state
        { currentSymbol = nextSymbol
        , nextSymbol = ""
        , currentFrame = newCurrentFrame chartContent
        , previousFrame = newPreviousFrame currentFrame
        }
  return $ FrameOutput
    { outputState = outputState
    , isDirty = True
    , addedSymbol = Nothing
    , selectedSymbol = Just nextSymbol
    }

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

-- Append to the next symbol if the key is just a character...
handleCharKey :: Key -> FrameState -> IO FrameOutput
handleCharKey key state@FrameState { nextSymbol = nextSymbol } =
  return $ FrameOutput
    { outputState = outputState
    , isDirty = isDirty
    , addedSymbol = Nothing
    , selectedSymbol = Nothing
    }
  where
    (outputState, isDirty) =
        case key of
          (CharKey char) ->
              if isAlpha char
                then (state { nextSymbol = nextSymbol ++ [char] }, True)
                else (state, False)
          _ -> (state, False)

-- ESC cancels the next symbol.
handleEscapeKey :: FrameState -> IO FrameOutput
handleEscapeKey state =
  return $ FrameOutput
    { outputState = state { nextSymbol = "" }
    , isDirty = True
    , addedSymbol = Nothing
    , selectedSymbol = Nothing
    }

handleNoKey :: FrameState -> IO FrameOutput
handleNoKey state =
  return $ FrameOutput
    { outputState = state
    , isDirty = False
    , addedSymbol = Nothing
    , selectedSymbol = Nothing
    }

drawFrames :: Resources -> Box -> FrameOutput -> IO FrameOutput
drawFrames resources bounds
    FrameOutput
      { outputState = outputState@FrameState
        { currentFrame = currentFrame
        , previousFrame = previousFrame
        }
      , isDirty = isDirty
      , selectedSymbol = selectedSymbol
      } = do
  maybeNextCurrentFrameState <- drawFrameShort currentFrame
  maybeNextPreviousFrameState <- drawFrameShort previousFrame
  let (nextCurrentFrame, isCurrentContentDirty, nextAddedSymbol) =
          maybeNextCurrentFrameState
      (nextPreviousFrame, isPreviousContentDirty, _) =
          maybeNextPreviousFrameState
      nextNextCurrentFrame = nextFrame currentFrame
      nextNextPreviousFrame = nextFrame previousFrame
      nextState = outputState 
        { currentFrame = nextNextCurrentFrame
        , previousFrame = nextNextPreviousFrame
        }
      bothFrames = [nextNextCurrentFrame, nextNextPreviousFrame]
      nextDirty = any id
        [ isDirty
        , any isDirtyFrame (catMaybes bothFrames)
        , isCurrentContentDirty
        , isPreviousContentDirty
        ]
      nextSelectedSymbol =
          if isJust selectedSymbol
            then selectedSymbol
            else nextAddedSymbol
  return FrameOutput
    { outputState = nextState
    , isDirty = nextDirty
    , addedSymbol = nextAddedSymbol
    , selectedSymbol = nextSelectedSymbol
    } 
  where
    drawFrameShort = drawFrame resources bounds

-- TODO: Check if the Chart's MVar is empty...
isDirtyFrame :: Frame -> Bool
isDirtyFrame (Frame
    { scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    }) = any (snd . current) [scaleAnimation, alphaAnimation]

drawFrame :: Resources -> Box -> Maybe Frame
    -> IO (Maybe Frame, Dirty, Maybe Symbol)

drawFrame resources _ Nothing = return (Nothing, False, Nothing)

drawFrame resources bounds
    (Just frame@Frame
      { content = content
      , scaleAnimation = scaleAnimation
      , alphaAnimation = alphaAnimation
      }) = 
  preservingMatrix $ do
    scale3 scaleAmount scaleAmount 1
    color $ outlineColor resources paddedBounds alphaAmount
    drawRoundedRectangle (boxWidth paddedBounds) (boxHeight paddedBounds)
        cornerRadius cornerVertices
    (nextContent, isDirty, addedSymbol) <- drawFrameContent resources
        bounds content alphaAmount
    return (Just frame { content = nextContent }, isDirty, addedSymbol)
  where
    paddedBounds = boxShrink bounds contentPadding
    scaleAmount = fst . current $ scaleAnimation
    alphaAmount = fst . current $ alphaAnimation

contentPadding = 5::GLfloat -- ^ Padding on one side.
cornerRadius = 10::GLfloat
cornerVertices = 5::Int

drawFrameContent :: Resources -> Box -> Content -> GLfloat
    -> IO (Content, Dirty, Maybe Symbol)

drawFrameContent resources bounds Instructions alpha = do
  output <- I.drawInstructions resources input
  return $ (Instructions, I.isDirty output, Nothing)
  where
    input = I.InstructionsInput
      { I.bounds = bounds
      , I.alpha = alpha
      }

drawFrameContent resources bounds (Chart symbol state) alpha = do
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

nextFrame :: Maybe Frame -> Maybe Frame
nextFrame Nothing = Nothing
nextFrame (Just frame) = Just $ frame
  { scaleAnimation = next $ scaleAnimation frame
  , alphaAnimation = next $ alphaAnimation frame
  }

drawNextSymbol :: Resources -> Box -> FrameOutput -> IO FrameOutput

drawNextSymbol _ _
    frameOutput@FrameOutput
      { outputState = FrameState { nextSymbol = "" }
      } =
  return frameOutput

drawNextSymbol resources bounds
    frameOutput@FrameOutput
      { outputState = FrameState { nextSymbol = nextSymbol }
      } = do

  boundingBox <- measureText textSpec
  let textBubbleWidth = boxWidth boundingBox + textBubblePadding * 2
      textBubbleHeight = boxHeight boundingBox + textBubblePadding * 2
      (centerX, centerY) = boxCenter boundingBox

  preservingMatrix $ do 
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

  return frameOutput

  where
    textSpec = TextSpec (font resources) 48  nextSymbol
    textBubblePadding = 15

