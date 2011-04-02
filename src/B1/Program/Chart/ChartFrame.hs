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
import qualified B1.Program.Chart.MiniChart as M

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
  , draggedOutMiniChart :: Maybe M.MiniChartState
  }

data FrameState = FrameState
  { currentSymbol :: String
  , nextSymbol :: String
  , currentFrame :: Maybe Frame
  , previousFrame :: Maybe Frame
  , draggedMiniChartState :: Maybe M.MiniChartState
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
  , draggedMiniChartState = Nothing
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
  drawDraggedChart resources bounds
      =<< drawNextSymbol resources bounds
      =<< drawFrames resources bounds
      =<< refreshSymbolState resources
      =<< convertFrameInput frameInput

convertFrameInput :: FrameInput -> IO FrameOutput
convertFrameInput
    frameInput@FrameInput
      { inputState = frameState
      , symbolRequest = symbolRequest
      } =
  return $ FrameOutput
    { outputState = frameState
    , isDirty = False
    , addedSymbol = Nothing
    , selectedSymbol = symbolRequest
    , draggedOutMiniChart = Nothing
    }

refreshSymbolState :: Resources -> FrameOutput -> IO FrameOutput

refreshSymbolState _
    frameOutput@FrameOutput
      { outputState = state
      , selectedSymbol = Just symbol
      } = 
  loadNextSymbol $ frameOutput { outputState = state { nextSymbol = symbol } }

refreshSymbolState resources frameOutput
  | checkKeyPress (SpecialKey BACKSPACE) = handleBackspaceKey frameOutput
  | checkKeyPress (SpecialKey ENTER) = handleEnterKey frameOutput
  | checkKeyPress (SpecialKey ESC) = handleEscapeKey frameOutput
  | isJust maybeLetterKey = handleCharKey (fromJust maybeLetterKey) frameOutput
  | otherwise = handleNoKey frameOutput
  where
    checkKeyPress = isKeyPressed resources
    maybeLetterKey = getKeyPressed resources $ map CharKey ['A'..'Z']

-- BACKSPACE deletes one character in a symbol...
handleBackspaceKey :: FrameOutput -> IO FrameOutput
handleBackspaceKey
    frameOutput@FrameOutput
      { outputState = state@FrameState { nextSymbol = nextSymbol }
      , isDirty = isDirty
      }  =
  return frameOutput
    { outputState = nextState
    , isDirty = nextIsDirty
    }
  where
    isNextSymbolEmpty = length nextSymbol < 1
    trimmedSymbol = take (length nextSymbol - 1) nextSymbol
    (nextState, nextIsDirty) =
        if isNextSymbolEmpty
          then (state, isDirty)
          else (state { nextSymbol = trimmedSymbol }, True)

-- ENTER makes the next symbol the current symbol.
handleEnterKey :: FrameOutput -> IO FrameOutput
handleEnterKey
    frameOutput@FrameOutput
      { outputState = state@FrameState { nextSymbol = nextSymbol }
      } = 
  if nextSymbol == ""
    then return frameOutput
    else loadNextSymbol frameOutput

loadNextSymbol :: FrameOutput -> IO FrameOutput
loadNextSymbol
    frameOutput@FrameOutput
      { outputState = state@FrameState
        { nextSymbol = nextSymbol
        , currentFrame = currentFrame
        }
      } = do
  chartContent <- newChartContent nextSymbol
  let nextOutputState = state
        { currentSymbol = nextSymbol
        , nextSymbol = ""
        , currentFrame = newCurrentFrame chartContent
        , previousFrame = newPreviousFrame currentFrame
        }
  return frameOutput
    { outputState = nextOutputState
    , isDirty = True
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
handleCharKey :: Key -> FrameOutput -> IO FrameOutput
handleCharKey key
    frameOutput@FrameOutput
      { outputState = state@FrameState { nextSymbol = nextSymbol } 
      } =
  return frameOutput
    { outputState = nextOutputState
    , isDirty = nextIsDirty
    }
  where
    (nextOutputState, nextIsDirty) =
        case key of
          (CharKey char) ->
              if isAlpha char
                then (state { nextSymbol = nextSymbol ++ [char] }, True)
                else (state, False)
          _ -> (state, False)

-- ESC cancels the next symbol.
handleEscapeKey :: FrameOutput -> IO FrameOutput
handleEscapeKey frameOutput@FrameOutput { outputState = state } =
  return frameOutput
    { outputState = state { nextSymbol = "" }
    , isDirty = True
    }

handleNoKey :: FrameOutput -> IO FrameOutput
handleNoKey = return

drawFrames :: Resources -> Box -> FrameOutput -> IO FrameOutput
drawFrames resources bounds
    frameOutput@FrameOutput
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
      nextNextCurrentFrame = nextFrame nextCurrentFrame
      nextNextPreviousFrame = nextFrame nextPreviousFrame
      nextState = outputState 
        { currentFrame = nextNextCurrentFrame
        , previousFrame = nextNextPreviousFrame
        }
      nextDirty = any id
        [ isDirty
        , isDirtyFrame nextNextCurrentFrame
        , isDirtyFrame nextNextPreviousFrame
        , isCurrentContentDirty
        , isPreviousContentDirty
        ]
      nextSelectedSymbol =
          if isJust selectedSymbol
            then selectedSymbol
            else nextAddedSymbol
  return frameOutput
    { outputState = nextState
    , isDirty = nextDirty
    , addedSymbol = nextAddedSymbol
    , selectedSymbol = nextSelectedSymbol
    } 
  where
    drawFrameShort = drawFrame resources bounds

-- TODO: Check if the Chart's MVar is empty...
isDirtyFrame :: Maybe Frame -> Bool
isDirtyFrame (Just (Frame
    { scaleAnimation = scaleAnimation
    , alphaAnimation = alphaAnimation
    })) = any (snd . current) [scaleAnimation, alphaAnimation]
isDirtyFrame _ = False

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

drawDraggedChart :: Resources -> Box -> FrameOutput -> IO FrameOutput
drawDraggedChart resources bounds
    frameOutput@FrameOutput
      { outputState = outputState@FrameState
        { currentFrame = currentFrame
        , draggedMiniChartState = draggedMiniChartState
        }
      , isDirty = isDirty
      }
  | draggingChartInBounds = do
      let Just (Frame { content = Chart symbol chartState }) = currentFrame
      miniChartState <- case draggedMiniChartState of
        Just state -> return $state
        _ -> M.newMiniChartState symbol $ Just (C.stockData chartState)
     
      let (mouseX, mouseY) = mousePosition resources
          miniChartBounds = createBox 150 100 (mouseX, mouseY)
          miniChartInput = M.MiniChartInput
            { M.bounds = miniChartBounds
            , M.alpha = 1.0
            , M.isBeingDragged = True
            , M.inputState = miniChartState
            } 
      miniChartOutput <- preservingMatrix $ do
        translate $ vector3 (-(boxLeft bounds + boxWidth bounds / 2))
            (-(boxBottom bounds + boxHeight bounds / 2))  0
        translate $ vector3 mouseX mouseY 0
        M.drawMiniChart resources miniChartInput
      return frameOutput
        { outputState = outputState
          { draggedMiniChartState = Just $ M.outputState miniChartOutput
          }
        , isDirty = isDirty || M.isDirty miniChartOutput
        }
  | draggingChartOutOfBounds = return frameOutput
      { draggedOutMiniChart = nextDraggedOutMiniChart
      }
  | otherwise = return frameOutput
      { outputState = outputState { draggedMiniChartState = Nothing }
      }
  where
    isCurrentFrameChart =
        case currentFrame of
          Just (Frame { content = Chart _ _ }) -> True
          otherwise -> False

    draggingChartInBounds = isMouseDrag resources
        && boxContains bounds (mouseDragStartPosition resources)
        && boxContains bounds (mousePosition resources)
        && isCurrentFrameChart

    previouslyDraggingChartInBounds = isMouseDrag resources
        && boxContains bounds (mouseDragStartPosition resources)
        && boxContains bounds (previousMousePosition resources)
        && isCurrentFrameChart

    draggingChartOutOfBounds = isMouseDrag resources
        && boxContains bounds (mouseDragStartPosition resources)
        && not (boxContains bounds (mousePosition resources))
        && isCurrentFrameChart

    nextDraggedOutMiniChart =
        if previouslyDraggingChartInBounds && draggingChartOutOfBounds
          then draggedMiniChartState
          else Nothing
