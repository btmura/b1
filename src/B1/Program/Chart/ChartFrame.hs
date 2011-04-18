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
  , justSelectedSymbol :: Maybe Symbol
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
incomingScaleAnimation = animateOnce $ linearRange 1.05 1 10

incomingAlphaAnimation :: Animation (GLfloat, Dirty)
incomingAlphaAnimation = animateOnce $ linearRange 0 1 10

outgoingScaleAnimation :: Animation (GLfloat, Dirty)
outgoingScaleAnimation = animateOnce $ linearRange 1 1.05 10

outgoingAlphaAnimation :: Animation (GLfloat, Dirty)
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 10

drawChartFrame :: Resources -> FrameInput -> IO FrameOutput
drawChartFrame resources input =
  convertInputToStuff input
      >>= refreshSymbolState resources
      >>= refreshSelectedSymbol
      >>= drawFrames resources
      >>= drawNextSymbol resources
      >>= drawDraggedChart resources
      >>= convertStuffToOutput

data FrameStuff = FrameStuff
  { frameBounds :: Box
  , frameRequestedSymbol :: Maybe Symbol
  , frameCurrentSymbol :: String
  , frameNextSymbol :: String
  , frameCurrentFrame :: Maybe Frame
  , framePreviousFrame :: Maybe Frame
  , frameAddedSymbol :: Maybe Symbol
  , frameSelectedSymbol :: Maybe Symbol
  , frameJustSelectedSymbol :: Maybe Symbol
  , frameDraggedChart :: Maybe M.MiniChartState
  , frameDraggedOutChart :: Maybe M.MiniChartState
  , frameIsDirty :: Dirty
  }

convertInputToStuff :: FrameInput -> IO FrameStuff
convertInputToStuff 
    FrameInput
      { bounds = bounds
      , symbolRequest = symbolRequest
      , inputState = FrameState
        { currentSymbol = currentSymbol
        , nextSymbol = nextSymbol
        , currentFrame = currentFrame
        , previousFrame = previousFrame
        , draggedMiniChartState = draggedMiniChartState
        }
      } =
  return FrameStuff
    { frameBounds = bounds
    , frameRequestedSymbol = symbolRequest
    , frameCurrentSymbol = currentSymbol
    , frameNextSymbol = nextSymbol
    , frameCurrentFrame = currentFrame
    , framePreviousFrame = previousFrame
    , frameAddedSymbol = Nothing
    , frameSelectedSymbol = Nothing
    , frameJustSelectedSymbol = Nothing
    , frameDraggedChart = draggedMiniChartState
    , frameDraggedOutChart = Nothing
    , frameIsDirty = False
    }

refreshSymbolState :: Resources -> FrameStuff -> IO FrameStuff

refreshSymbolState _ stuff@FrameStuff { frameRequestedSymbol = Just symbol } =
  loadSymbol symbol stuff

refreshSymbolState resources stuff
  | checkKeyPress (SpecialKey BACKSPACE) = handleBackspaceKey stuff
  | checkKeyPress (SpecialKey ENTER) = handleEnterKey stuff
  | checkKeyPress (SpecialKey ESC) = handleEscapeKey stuff
  | isJust maybeLetterKey = handleCharKey (fromJust maybeLetterKey) stuff
  | otherwise = return stuff
  where
    checkKeyPress = isKeyPressed resources
    maybeLetterKey = getKeyPressed resources $ map CharKey ['A'..'Z']

-- BACKSPACE deletes one character in a symbol...
handleBackspaceKey :: FrameStuff -> IO FrameStuff
handleBackspaceKey
    stuff@FrameStuff
      { frameNextSymbol = nextSymbol
      , frameIsDirty = isDirty
      } =
  return stuff
    { frameNextSymbol = nextNextSymbol
    , frameIsDirty = isDirty || not isCurrentNextSymbolEmpty
    }
  where
    isCurrentNextSymbolEmpty = length nextSymbol < 1
    nextNextSymbol =
        if isCurrentNextSymbolEmpty
          then nextSymbol
          else take (length nextSymbol - 1) nextSymbol

-- ENTER makes the next symbol the current symbol.
handleEnterKey :: FrameStuff -> IO FrameStuff
handleEnterKey stuff@FrameStuff { frameNextSymbol = nextSymbol } =
  if nextSymbol == ""
    then return stuff
    else loadSymbol nextSymbol stuff
 
loadSymbol :: Symbol -> FrameStuff -> IO FrameStuff
loadSymbol symbol
    stuff@FrameStuff { frameCurrentFrame = currentFrame } = do
  chartContent <- newChartContent symbol
  return stuff
    { frameCurrentSymbol = symbol
    , frameNextSymbol = ""
    , frameCurrentFrame = newCurrentFrame chartContent
    , framePreviousFrame = newPreviousFrame currentFrame
    , frameJustSelectedSymbol = Just symbol
    , frameIsDirty = True
    , frameDraggedChart = Nothing
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
handleCharKey :: Key -> FrameStuff -> IO FrameStuff
handleCharKey key
    stuff@FrameStuff
      { frameNextSymbol = nextSymbol
      , frameIsDirty = isDirty
      } =
  return stuff
    { frameNextSymbol = nextNextSymbol
    , frameIsDirty = nextIsDirty
    }
  where
    (nextNextSymbol, nextIsDirty) =
        case key of
          (CharKey char) ->
              if isAlpha char
                then (nextSymbol ++ [char], True)
                else (nextSymbol, isDirty)
          _ -> (nextSymbol, isDirty)

-- ESC cancels the next symbol.
handleEscapeKey :: FrameStuff -> IO FrameStuff
handleEscapeKey stuff = 
  return stuff
    { frameNextSymbol = ""
    , frameIsDirty = True
    }

refreshSelectedSymbol :: FrameStuff -> IO FrameStuff
refreshSelectedSymbol stuff@FrameStuff { frameCurrentFrame = currentFrame } =
  return stuff { frameSelectedSymbol = nextSelectedSymbol }
  where
    nextSelectedSymbol =
        case currentFrame of
          Just frame ->
              case content frame of
                Chart symbol _ -> Just symbol
                _ -> Nothing
          _ -> Nothing

drawFrames :: Resources -> FrameStuff -> IO FrameStuff
drawFrames resources 
    stuff@FrameStuff
      { frameBounds = bounds
      , frameCurrentFrame = currentFrame
      , framePreviousFrame = previousFrame
      , frameJustSelectedSymbol = justSelectedSymbol
      , frameIsDirty = isDirty
      } = do

  maybeNextCurrentFrameState <- drawFrameShort currentFrame
  maybeNextPreviousFrameState <- drawFrameShort previousFrame

  let (nextCurrentFrame, isCurrentContentDirty, nextAddedSymbol) =
          maybeNextCurrentFrameState
      (nextPreviousFrame, isPreviousContentDirty, _) =
          maybeNextPreviousFrameState
      nextNextCurrentFrame = nextFrame nextCurrentFrame
      nextNextPreviousFrame = nextFrame nextPreviousFrame
      nextJustSelectedSymbol =
          if isJust nextAddedSymbol
            then nextAddedSymbol
            else justSelectedSymbol
      nextDirty = any id
        [ isDirty
        , isDirtyFrame nextNextCurrentFrame
        , isDirtyFrame nextNextPreviousFrame
        , isCurrentContentDirty
        , isPreviousContentDirty
        ]
  return stuff
    { frameCurrentFrame = nextNextCurrentFrame
    , framePreviousFrame = nextNextPreviousFrame
    , frameAddedSymbol = nextAddedSymbol
    , frameJustSelectedSymbol = nextJustSelectedSymbol
    , frameIsDirty = nextDirty
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

drawFrameContent _ _ content 0 = return (content, False, Nothing)

drawFrameContent resources bounds Instructions alpha = do
  output <- I.drawInstructions resources input
  return (Instructions, I.isDirty output, Nothing)
  where
    input = I.InstructionsInput
      { I.bounds = bounds
      , I.alpha = alpha
      }

drawFrameContent resources bounds (Chart symbol state) alpha = do
  output <- C.drawChart resources input
  return (Chart symbol (C.outputState output), C.isDirty output,
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

drawNextSymbol :: Resources -> FrameStuff -> IO FrameStuff

drawNextSymbol _ stuff@FrameStuff { frameNextSymbol = "" } =
  return stuff

drawNextSymbol resources 
    stuff@FrameStuff
      { frameBounds = bounds
      , frameNextSymbol = nextSymbol
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

  return stuff

  where
    textSpec = TextSpec (font resources) 48  nextSymbol
    textBubblePadding = 15

drawDraggedChart :: Resources -> FrameStuff -> IO FrameStuff
drawDraggedChart resources
    stuff@FrameStuff
      { frameBounds = bounds
      , frameCurrentFrame = currentFrame
      , frameDraggedChart = draggedMiniChartState
      , frameIsDirty = isDirty
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
      return stuff
        { frameDraggedChart = Just $ M.outputState miniChartOutput
        , frameIsDirty = isDirty || M.isDirty miniChartOutput
        }
  | draggingChartOutOfBounds =
      return stuff { frameDraggedOutChart = nextDraggedOutMiniChart }
  | otherwise =
      return stuff { frameDraggedChart = Nothing }
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

convertStuffToOutput :: FrameStuff -> IO FrameOutput
convertStuffToOutput
    FrameStuff
      { frameCurrentSymbol = currentSymbol
      , frameNextSymbol = nextSymbol
      , frameCurrentFrame = currentFrame
      , framePreviousFrame = previousFrame
      , frameAddedSymbol = addedSymbol
      , frameSelectedSymbol = selectedSymbol
      , frameJustSelectedSymbol = justSelectedSymbol
      , frameIsDirty = isDirty
      , frameDraggedChart = draggedChart
      , frameDraggedOutChart = draggedOutChart
      } =
  return FrameOutput
    { outputState = FrameState
      { currentSymbol = currentSymbol
      , nextSymbol = nextSymbol
      , currentFrame = currentFrame
      , previousFrame = previousFrame
      , draggedMiniChartState = draggedChart
      }
    , isDirty = isDirty
    , addedSymbol = addedSymbol
    , selectedSymbol = selectedSymbol
    , justSelectedSymbol = justSelectedSymbol
    , draggedOutMiniChart = draggedOutChart
    }

