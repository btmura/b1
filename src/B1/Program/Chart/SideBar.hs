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
  { isDirty :: Dirty
  , symbolRequest :: Maybe Symbol
  , outputState :: SideBarState
  }

data SideBarState = SideBarState
  { slots :: [Slot]
  }

data Slot = Slot
  { symbol :: Symbol
  , remove :: Bool
  , heightAnimation :: Animation (GLfloat, Dirty)
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
      nextSlots = filter (not . shouldRemoveSlot) $
          map (uncurry updateMiniChartState) (zip updatedSlots outputStates)
      nextState = SideBarState { slots = nextSlots } 
      isSideBarDirty = isJust maybeNewSymbol
          || any M.isDirty outputStates
          || any id dirtySlots
      nextSymbolRequest = (listToMaybe
          . catMaybes
          . map M.symbolRequest) outputStates

  return SideBarOutput
    { isDirty = isSideBarDirty
    , symbolRequest = nextSymbolRequest
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
        { symbol = newSymbol
        , remove = False
        , heightAnimation = incomingHeightAnimation
        , alphaAnimation = incomingAlphaAnimation
        , scaleAnimation = incomingScaleAnimation
        , miniChartState = miniChartState
        }]
  where
    alreadyAdded = any (containsSymbol newSymbol) slots

containsSymbol :: Symbol -> Slot -> Bool
containsSymbol newSymbol slot = (symbol slot) == newSymbol && not (remove slot)

incomingHeightAnimation = animateOnce $ linearRange 100 100 20
incomingAlphaAnimation = animateOnce $ linearRange 0 1 20
incomingScaleAnimation = animateOnce $ linearRange 1 1 20
outgoingHeightAnimation = animateOnce $ linearRange 100 0 20
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
    return (output, alphaDirty || scaleDirty || heightDirty)
  where
    (height, heightDirty) = current $ heightAnimation slot
    (alpha, alphaDirty) = current $ alphaAnimation slot
    (scale, scaleDirty) = current $ scaleAnimation slot

    slotsAbove = take index slots
    heightAbove = sum $ map (fst . current . heightAnimation) slotsAbove

    slot = slots !! index
    slotTop = top - heightAbove
    slotHeight = 100
    slotBottom = slotTop - slotHeight
    slotBounds = Box (left, slotTop) (right, slotBottom)
    input = M.MiniChartInput
      { M.bounds = slotBounds
      , M.alpha = alpha
      , M.symbol = symbol slot
      , M.inputState = miniChartState slot
      }

shouldRemoveSlot :: Slot -> Bool
shouldRemoveSlot slot@Slot
    { remove = remove
    , heightAnimation = heightAnimation
    } =
  remove && (fst . current) heightAnimation == 0

updateMiniChartState :: Slot -> M.MiniChartOutput -> Slot
updateMiniChartState
    slot@Slot
      { remove = alreadyRemoved
      , heightAnimation = heightAnimation
      , alphaAnimation = alphaAnimation
      , scaleAnimation = scaleAnimation
      }
    M.MiniChartOutput
      { M.outputState = nextState
      , M.removeChart = removeNow
      } = slot
  { remove = alreadyRemoved || removeNow
  , heightAnimation = nextHeightAnimation
  , alphaAnimation = nextAlphaAnimation
  , scaleAnimation = nextScaleAnimation
  , miniChartState = nextState
  }
  where
    (nextHeightAnimation, nextAlphaAnimation, nextScaleAnimation) =
        if removeNow
          then (outgoingHeightAnimation, outgoingAlphaAnimation,
              outgoingScaleAnimation)
          else (next heightAnimation, next alphaAnimation,
              next scaleAnimation)

