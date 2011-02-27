module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , SideBarState(..)
  , drawSideBar
  , newSideBarState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.MiniChart as M

data SideBarInput = SideBarInput
  { bounds :: Box
  , maybeNewSymbol :: Maybe Symbol
  , inputState :: SideBarState
  }

data SideBarOutput = SideBarOutput
  { outputState :: SideBarState
  , isDirty :: Dirty
  }

data SideBarState = SideBarState
  { slots :: [Slot]
  }

data Slot = Slot
  { symbol :: Symbol
  , height :: GLfloat
  , alphaAnimation :: Animation (GLfloat, Dirty)
  , scaleAnimation :: Animation (GLfloat, Dirty)
  , miniChartState :: M.MiniChartState
  } 

newSideBarState :: SideBarState
newSideBarState  = SideBarState
  { slots = []
  }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar resources
    SideBarInput
      { bounds = bounds
      , maybeNewSymbol = maybeNewSymbol
      , inputState = SideBarState { slots = slots }
      } = do

  updatedSlots <- addSymbol maybeNewSymbol slots

  let indices = [0 .. length updatedSlots - 1]
  output <- mapM (drawSlot resources bounds updatedSlots) indices

  let (outputStates, dirtySlots) = unzip output
      nextSlots = map (uncurry updateMiniChartState) 
          (zip updatedSlots outputStates)
      nextState = SideBarState { slots = nextSlots } 
      isSideBarDirty = isJust maybeNewSymbol
          || any M.isDirty outputStates
          || any id dirtySlots
  return SideBarOutput
    { isDirty = isSideBarDirty
    , outputState = nextState
    }

addSymbol :: Maybe Symbol -> [Slot] -> IO [Slot]
addSymbol Nothing slots = return $ slots
addSymbol (Just newSymbol) slots =
  if alreadyAdded
    then return $ slots
    else do
      miniChartState <- M.newMiniChartState newSymbol
      return $ slots ++ [Slot
        { height = 100
        , symbol = newSymbol
        , alphaAnimation = incomingAlphaAnimation
        , scaleAnimation = incomingScaleAnimation
        , miniChartState = miniChartState
        }]
  where
    alreadyAdded = any ((== newSymbol) . symbol) slots

incomingAlphaAnimation = animateOnce $ linearRange 0 1 20
incomingScaleAnimation = animateOnce $ linearRange 1 1 20
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 20
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 20
   
drawSlot :: Resources -> Box -> [Slot] -> Int -> IO (M.MiniChartOutput, Dirty)
drawSlot resources bounds@(Box (left, top) (right, bottom)) slots index = 
  preservingMatrix $ do
    -- Translate to the bottom from the center
    translate $ vector3 0 (-(boxHeight bounds / 2)) 0

    -- Translate back up to the center of the slot
    translate $ vector3 0 (slotTop - slotHeight / 2) 0

    scale3 scale scale 1
    output <- M.drawMiniChart resources input
    return (output, alphaDirty || scaleDirty)
  where
    slotsAbove = take index slots
    heightAbove = sum $ map height slotsAbove

    slot = slots !! index
    slotTop = top - heightAbove
    slotHeight = height slot
    slotBottom = slotTop - slotHeight
    slotBounds = Box (left, slotTop) (right, slotBottom)
    (alpha, alphaDirty) = current $ alphaAnimation slot
    (scale, scaleDirty) = current $ scaleAnimation slot
    input = M.MiniChartInput
      { M.bounds = slotBounds
      , M.alpha = alpha
      , M.symbol = symbol slot
      , M.inputState = miniChartState slot
      }

updateMiniChartState :: Slot -> M.MiniChartOutput -> Slot
updateMiniChartState
    slot@Slot
      { alphaAnimation = alphaAnimation
      , scaleAnimation = scaleAnimation
      }
    M.MiniChartOutput
      { M.outputState = nextState
      , M.removeChart = removeChart
      } = slot
  { alphaAnimation = nextAlphaAnimation
  , scaleAnimation = nextScaleAnimation
  , miniChartState = nextState
  }
  where
    (nextAlphaAnimation, nextScaleAnimation) = if removeChart
      then (outgoingAlphaAnimation, outgoingScaleAnimation)
      else (next alphaAnimation, next scaleAnimation)

