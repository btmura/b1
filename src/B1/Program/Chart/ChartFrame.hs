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
import B1.Data.Price.Google
import B1.Data.Range
import B1.Data.Symbol
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.Instructions as I
import qualified B1.Program.Chart.MiniChart as M

data FrameInput = FrameInput
  { bounds :: Box
  , symbolRequests :: [Symbol]
  , inputState :: FrameState
  }

data FrameOutput = FrameOutput
  { outputState :: FrameState
  , isDirty :: Dirty
  , addedSymbol :: Maybe Symbol -- ^ Symbol when add button clicked
  , refreshedSymbol :: Maybe Symbol -- ^ Symbol when refresh button clicked
  , selectedSymbol :: Maybe Symbol
  }

data FrameState = FrameState
  { currentFrame :: Maybe Frame
  , previousFrame :: Maybe Frame
  }

data Frame = Frame
  { content :: Content
  , removing :: Bool
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

data Content = Instructions | Chart Symbol C.ChartState

newFrameState :: FrameState
newFrameState = FrameState
  { currentFrame = Just Frame
    { content = Instructions
    , removing = False
    , scaleAnimation = incomingScaleAnimation
    , alphaAnimation = incomingAlphaAnimation
    }
  , previousFrame = Nothing
  }

incomingScaleAnimation :: Animation (GLfloat, Dirty)
incomingScaleAnimation = animateOnce $ linearRange 1.25 1 20

incomingAlphaAnimation :: Animation (GLfloat, Dirty)
incomingAlphaAnimation = animateOnce $ linearRange 0 1 20

outgoingScaleAnimation :: Animation (GLfloat, Dirty)
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 20

outgoingAlphaAnimation :: Animation (GLfloat, Dirty)
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 20

drawChartFrame :: Resources -> FrameInput -> IO FrameOutput
drawChartFrame resources input =
  convertInputToStuff input
      >>= refreshSymbolState resources
      >>= drawFrames resources
      >>= convertStuffToOutput

data FrameStuff = FrameStuff
  { frameBounds :: Box
  , frameSymbolRequests :: [Symbol]
  , frameCurrentFrame :: Maybe Frame
  , framePreviousFrame :: Maybe Frame
  , frameAddedSymbol :: Maybe Symbol
  , frameRefreshedSymbol :: Maybe Symbol
  , frameJustSelectedSymbol :: Maybe Symbol
  , frameIsDirty :: Dirty
  }

convertInputToStuff :: FrameInput -> IO FrameStuff
convertInputToStuff 
    FrameInput
      { bounds = bounds
      , symbolRequests = symbolRequests
      , inputState = FrameState
        { currentFrame = currentFrame
        , previousFrame = previousFrame
        }
      } =
  return FrameStuff
    { frameBounds = bounds
    , frameSymbolRequests = symbolRequests
    , frameCurrentFrame = currentFrame
    , framePreviousFrame = previousFrame
    , frameAddedSymbol = Nothing
    , frameRefreshedSymbol = Nothing
    , frameJustSelectedSymbol = Nothing
    , frameIsDirty = False
    }

refreshSymbolState :: Resources -> FrameStuff -> IO FrameStuff

refreshSymbolState _
  stuff@FrameStuff
    { frameSymbolRequests = (symbol:_)
    , frameCurrentFrame = currentFrame
    } = do
  chartContent <- newChartContent symbol
  return stuff
    { frameCurrentFrame = newCurrentFrame chartContent
    , framePreviousFrame = newPreviousFrame currentFrame
    , frameJustSelectedSymbol = Just symbol
    , frameIsDirty = True
    }

refreshSymbolState _ stuff = return stuff

newChartContent :: Symbol -> IO Content
newChartContent symbol = do
  state <- C.newChartState symbol
  return $ Chart symbol state

newCurrentFrame :: Content -> Maybe Frame
newCurrentFrame content = Just Frame
  { content = content
  , removing = False
  , scaleAnimation = incomingScaleAnimation
  , alphaAnimation = incomingAlphaAnimation
  }

newPreviousFrame :: Maybe Frame -> Maybe Frame
newPreviousFrame Nothing = Nothing
newPreviousFrame (Just frame) = Just $ frame 
  { removing = True
  , scaleAnimation = outgoingScaleAnimation
  , alphaAnimation = outgoingAlphaAnimation
  }

drawFrames :: Resources -> FrameStuff -> IO FrameStuff
drawFrames resources 
    stuff@FrameStuff
      { frameBounds = bounds
      , frameCurrentFrame = currentFrame
      , framePreviousFrame = previousFrame
      , frameJustSelectedSymbol = selectedSymbol
      , frameIsDirty = isDirty
      } = do

  maybeNextCurrentFrameState <- drawFrameShort currentFrame
  maybeNextPreviousFrameState <- drawFrameShort previousFrame

  let (nextCurrentFrame, isCurrentContentDirty, nextAddedSymbol,
          nextRefreshedSymbol) = maybeNextCurrentFrameState
      (nextPreviousFrame, isPreviousContentDirty, _, _) =
          maybeNextPreviousFrameState

  nextNextCurrentFrame <- nextFrame nextCurrentFrame
  nextNextPreviousFrame <-  nextFrame nextPreviousFrame

  let nextJustSelectedSymbol =
          if isJust nextAddedSymbol
            then nextAddedSymbol
            else selectedSymbol
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
    , frameRefreshedSymbol = nextRefreshedSymbol
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
    -> IO (Maybe Frame, Dirty, Maybe Symbol, Maybe Symbol)

drawFrame resources _ Nothing = return (Nothing, False, Nothing, Nothing)

drawFrame resources bounds
    (Just frame@Frame
      { content = content
      , scaleAnimation = scaleAnimation
      , alphaAnimation = alphaAnimation
      }) = 
  preservingMatrix $ do
    scale3 scaleAmount scaleAmount 1
    (nextContent, isDirty, addedSymbol, refreshedSymbol)
        <- drawFrameContent resources bounds content alphaAmount
    return (Just frame { content = nextContent }, isDirty, addedSymbol,
        refreshedSymbol)
  where
    paddedBounds = boxShrink contentPadding bounds
    scaleAmount = fst . current $ scaleAnimation
    alphaAmount = fst . current $ alphaAnimation

contentPadding = 5::GLfloat -- ^ Padding on one side.
cornerRadius = 10::GLfloat
cornerVertices = 5::Int

drawFrameContent :: Resources -> Box -> Content -> GLfloat
    -> IO (Content, Dirty, Maybe Symbol, Maybe Symbol)

drawFrameContent _ _ content 0 = return (content, False, Nothing, Nothing)

drawFrameContent resources bounds Instructions alpha = do
  output <- I.drawInstructions resources input
  return (Instructions, I.isDirty output, Nothing, Nothing)
  where
    input = I.InstructionsInput
      { I.bounds = bounds
      , I.alpha = alpha
      }

drawFrameContent resources bounds (Chart symbol state) alpha = do
  output <- C.drawChart resources input
  return (Chart symbol (C.outputState output), C.isDirty output,
      C.addedSymbol output, C.refreshedSymbol output)
  where
    input = C.ChartInput
      { C.bounds = boxShrink contentPadding bounds
      , C.alpha = alpha
      , C.symbol = symbol
      , C.inputState = state
      }

nextFrame :: Maybe Frame -> IO (Maybe Frame)
nextFrame Nothing = return Nothing
nextFrame (Just frame) =
  if shouldRemoveFrame 
    then do
      cleanFrameContent $ content frame
      return Nothing
    else
      return $ Just nextFrame
  where
    shouldRemoveFrame = removing frame
        && (fst . current . alphaAnimation) frame == 0.0
    nextScaleAnimation = next $ scaleAnimation frame
    nextAlphaAnimation = next $ alphaAnimation frame
    nextFrame = frame
      { scaleAnimation = nextScaleAnimation
      , alphaAnimation = nextAlphaAnimation
      }

cleanFrameContent :: Content -> IO () 
cleanFrameContent (Chart symbol state) = do
  C.cleanChartState state
  return ()

cleanFrameContent _ = return ()

convertStuffToOutput :: FrameStuff -> IO FrameOutput
convertStuffToOutput
    FrameStuff
      { frameCurrentFrame = currentFrame
      , framePreviousFrame = previousFrame
      , frameAddedSymbol = addedSymbol
      , frameRefreshedSymbol = refreshedSymbol
      , frameJustSelectedSymbol = selectedSymbol
      , frameIsDirty = isDirty
      } =
  return FrameOutput
    { outputState = FrameState
      { currentFrame = currentFrame
      , previousFrame = previousFrame
      }
    , isDirty = isDirty
    , addedSymbol = addedSymbol
    , refreshedSymbol = refreshedSymbol
    , selectedSymbol = selectedSymbol
    }

