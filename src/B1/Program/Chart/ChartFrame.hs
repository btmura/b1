module B1.Program.Chart.ChartFrame
  ( FrameInput(..)
  , FrameOutput(..)
  , FrameOptions(..)
  , FrameState
  , drawChartFrame
  , newFrameState
  ) where

import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Data.Symbol
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.Header as H
import qualified B1.Program.Chart.Instructions as I

data FrameInput = FrameInput
  { bounds :: Box
  , maybeSymbolRequest :: Maybe Symbol
  , inputState :: FrameState
  }

data FrameOutput = FrameOutput
  { outputState :: FrameState
  , isDirty :: Dirty
  , buttonClickedSymbol :: Maybe Symbol -- ^ Symbol when button clicked
  , refreshedSymbol :: Maybe Symbol -- ^ Symbol when refresh button clicked
  , otherClickedSymbol :: Maybe Symbol -- ^ Symbol when other area clicked
  , selectedSymbol :: Maybe Symbol -- ^ Symbol when a request is fulfilled
  }

data FrameOptions = FrameOptions
  { initialSymbol :: Maybe Symbol -- ^ Symbol or nothing to get instructions
  , headerFontSize :: Int -- ^ Header font size
  }

data FrameState = FrameState
  { createChart :: Symbol -> IO Content
  , maybeCurrentFrame :: Maybe Frame
  , maybePreviousFrame :: Maybe Frame
  }

data Frame = Frame
  { content :: Content
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , alphaAnimation :: Animation (GLfloat, Dirty)
  }

data Content = Instructions | Chart Symbol C.ChartState

newFrameState :: C.ChartOptions -> Maybe Symbol -> IO FrameState
newFrameState chartOptions maybeSymbol = do
  let createChart = newChartContent chartOptions
  content <- case maybeSymbol of
    Just symbol -> createChart symbol
    _ -> return Instructions
  return FrameState
    { createChart = createChart
    , maybeCurrentFrame = Just Frame
      { content = content
      , scaleAnimation = incomingScaleAnimation
      , alphaAnimation = incomingAlphaAnimation
      }
    , maybePreviousFrame = Nothing
    }

drawChartFrame :: Resources -> FrameInput -> IO FrameOutput
drawChartFrame resources
    FrameInput
      { bounds = bounds
      , maybeSymbolRequest = maybeSymbolRequest
      , inputState = inputState
      } = do

  (revisedState, selectedSymbol) <- handleSymbolRequest
      maybeSymbolRequest inputState

  let FrameState
        { maybeCurrentFrame = maybeCurrentFrame
        , maybePreviousFrame = maybePreviousFrame
        } = revisedState
      render = renderFrame resources bounds
  (nextMaybeCurrentFrame, isCurrentDirty, buttonClickedSymbol,
      refreshedSymbol, otherClickedSymbol) <- render maybeCurrentFrame True
  (nextMaybePreviousFrame, isPreviousDirty, _,
      _, _) <- render maybePreviousFrame False

  let outputState = inputState
        { maybeCurrentFrame = nextMaybeCurrentFrame
        , maybePreviousFrame = nextMaybePreviousFrame
        }
      isDirty = isCurrentDirty || isPreviousDirty
  return FrameOutput
    { outputState = outputState
    , isDirty = isDirty
    , buttonClickedSymbol = buttonClickedSymbol
    , refreshedSymbol = refreshedSymbol
    , otherClickedSymbol = otherClickedSymbol
    , selectedSymbol = selectedSymbol
    }

handleSymbolRequest :: Maybe Symbol -> FrameState
    -> IO (FrameState, Maybe Symbol)
handleSymbolRequest Nothing state = return (state, Nothing)
handleSymbolRequest (Just symbol)
    state@FrameState
      { createChart = createChart
      , maybeCurrentFrame = maybeCurrentFrame
      } = do
  chartContent <- createChart symbol
  let outputState = state
        { maybeCurrentFrame = newCurrentFrame chartContent
        , maybePreviousFrame = newPreviousFrame maybeCurrentFrame
        } 
  return (outputState, Just symbol)

newChartContent :: C.ChartOptions -> Symbol -> IO Content
newChartContent chartOptions symbol = do
  state <- C.newChartState chartOptions symbol
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

renderFrame :: Resources -> Box -> Maybe Frame -> Bool
    -> IO (Maybe Frame, Dirty, Maybe Symbol, Maybe Symbol, Maybe Symbol)
renderFrame _ _ Nothing _ = return (Nothing, False, Nothing, Nothing, Nothing)
renderFrame resources bounds (Just frame) isCurrentFrame = do
  let currentContent = content frame
      currentScaleAnimation = scaleAnimation frame
      currentAlphaAnimation = alphaAnimation frame
      scaleAmount = (fst . current) currentScaleAnimation
      alphaAmount = (fst . current) currentAlphaAnimation
      removeInvisiblePreviousFrame = not isCurrentFrame && alphaAmount <= 0.0
  if removeInvisiblePreviousFrame
    then do
      cleanFrameContent currentContent
      return (Nothing, False, Nothing, Nothing, Nothing)
    else
      preservingMatrix $ do
        scale3 scaleAmount scaleAmount 1
        (nextContent, isContentDirty, buttonClickedSymbol, refreshedSymbol, 
            otherClickedSymbol) <- renderContent resources bounds
                currentContent alphaAmount
        let nextScaleAnimation = next currentScaleAnimation
            nextAlphaAnimation = next currentAlphaAnimation
            nextFrame = frame
              { content = nextContent
              , scaleAnimation = nextScaleAnimation
              , alphaAnimation = nextAlphaAnimation
              }
            isDirty = isContentDirty
                ||  (snd . current) currentScaleAnimation
                || (snd . current) currentAlphaAnimation
        return (Just nextFrame, isDirty, buttonClickedSymbol, refreshedSymbol,
            otherClickedSymbol)

-- TODO: Use type classes instead of special cases...
cleanFrameContent :: Content -> IO () 
cleanFrameContent (Chart symbol state) = do
  C.cleanChartState state
  return ()
cleanFrameContent _ = return ()

renderContent :: Resources -> Box -> Content -> GLfloat
    -> IO (Content, Dirty, Maybe Symbol, Maybe Symbol, Maybe Symbol)

renderContent resources bounds Instructions alpha = do
  let input = I.InstructionsInput
        { I.bounds = bounds
        , I.alpha = alpha
        }
  output <- I.drawInstructions resources input
  return (Instructions, I.isDirty output, Nothing, Nothing, Nothing)

renderContent resources bounds (Chart symbol state) alpha = do
  let input = C.ChartInput
        { C.bounds = boxShrink 5 bounds
        , C.alpha = alpha
        , C.inputState = state
        }
  output <- C.drawChart resources input
  return (Chart symbol (C.outputState output),
      C.isDirty output,
      C.buttonClickedSymbol output,
      C.refreshedSymbol output,
      C.otherClickedSymbol output)

