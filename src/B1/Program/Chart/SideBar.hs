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
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

import qualified B1.Program.Chart.MiniChart as M

slotHeight = 100

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
  , isBeingDragged :: Bool
  , dragOffset :: GLfloat
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
      , inputState = SideBarState { slots = inputSlots }
      } = do

  maybeNewSlot <- createSlot maybeNewSymbol

  let slots = (reorderSlotsBeingDragged resources bounds
          . markSlotsBeingDragged resources bounds 
          . addNewSlot maybeNewSlot) inputSlots

  output <- drawSlots resources bounds slots

  let (outputStates, dirtyFlags) = unzip output
      nextSlots = filter (not . shouldRemoveSlot) $
          map (uncurry updateMiniChartState) (zip slots outputStates)
      nextState = SideBarState { slots = nextSlots } 
      isSideBarDirty = isJust maybeNewSymbol
          || any M.isDirty outputStates
          || any id dirtyFlags
      nextSymbolRequest = (listToMaybe
          . catMaybes
          . map M.symbolRequest) outputStates

  return SideBarOutput
    { isDirty = isSideBarDirty
    , symbolRequest = nextSymbolRequest
    , outputState = nextState
    }

createSlot :: Maybe Symbol -> IO (Maybe Slot)
createSlot Nothing = return Nothing
createSlot (Just symbol) = do
  miniChartState <- M.newMiniChartState symbol
  return $ Just (Slot
    { symbol = symbol
    , remove = False
    , isBeingDragged = False
    , dragOffset = 0
    , heightAnimation = incomingHeightAnimation
    , alphaAnimation = incomingAlphaAnimation
    , scaleAnimation = incomingScaleAnimation
    , miniChartState = miniChartState
    })

addNewSlot :: Maybe Slot -> [Slot] -> [Slot]
addNewSlot Nothing slots = slots
addNewSlot (Just newSlot) slots
  | alreadyAdded = slots
  | otherwise = slots ++ [newSlot]
  where
    alreadyAdded = any (containsSymbol (symbol newSlot)) slots

containsSymbol :: Symbol -> Slot -> Bool
containsSymbol newSymbol slot = (symbol slot) == newSymbol && not (remove slot)

markSlotsBeingDragged :: Resources -> Box -> [Slot] -> [Slot]
markSlotsBeingDragged resources bounds slots = markedSlots
  where
    markedSlots = map (updateIsBeingDragged resources bounds slots)
        [0 .. length slots - 1] 

updateIsBeingDragged :: Resources -> Box-> [Slot] -> Int -> Slot
updateIsBeingDragged resources bounds slots index
  | hasMouseDragFinished resources = slot
      { isBeingDragged = False
      , dragOffset = 0
      }
  | hasMouseDragStarted resources = slot
      { isBeingDragged = beingDragged
      , dragOffset = dragOffset
      }
  | otherwise = slot
  where
    slot = slots !! index
    (beingDragged, dragOffset) = isDraggingSlot resources bounds slots index

isDraggingSlot :: Resources -> Box -> [Slot] -> Int -> (Bool, GLfloat)
isDraggingSlot resources bounds slots index = (slotDragged, dragOffset)
  where
    slotBounds = getSlotBounds bounds slots index
    dragStartPosition = mouseDragStartPosition resources
    slotDragged = isMouseDrag resources
        && boxContains slotBounds dragStartPosition
    dragOffset = boxTop slotBounds - snd dragStartPosition

getSlotBounds :: Box -> [Slot] -> Int -> Box
getSlotBounds (Box (left, top) (right, _)) slots index =
  slotBounds
  where
    slotsAbove = take index slots
    heightAbove = sum $ map (fst . current . heightAnimation) slotsAbove

    slot = slots !! index
    slotTop = top - heightAbove
    slotBottom = slotTop - slotHeight
    slotBounds = Box (left, slotTop) (right, slotBottom)

reorderSlotsBeingDragged :: Resources -> Box -> [Slot] -> [Slot]
reorderSlotsBeingDragged resources bounds slots
  | length slots == 1 = slots
  | length indexedDragSlots == 0 = slots
  | isMouseDrag resources = reorderedSlots
  | otherwise = slots
  where
    indexedSlots = zip [0..] slots
    indexedDragSlots = filter (isBeingDragged . snd) indexedSlots
    (dragIndex, dragSlot) = head indexedDragSlots
    dragPoint = boxCenter $ getDraggedBounds resources bounds slots dragIndex
    reorderedSlots = swapSlots resources bounds slots dragPoint dragIndex

swapSlots :: Resources -> Box -> [Slot] -> Point -> Int -> [Slot]
swapSlots resources bounds slots dragPoint dragIndex = swappedSlots
  where
    allSlotBounds = map (getSlotBounds bounds slots) [0 .. length slots - 1] 
    indexedSlotBounds = zip [0..] allSlotBounds
    containingSlots = filter (\(_, slotBounds) ->
        boxContains slotBounds dragPoint) indexedSlotBounds
    swapIndex =
        case containingSlots of
          [] ->  length slots - 1
          _ -> (fst . head) containingSlots
    swapSlot = slots !! swapIndex
    dragSlot = slots !! dragIndex
    swappedSlots =
      map (\(index, slot) ->
          if index == swapIndex
            then dragSlot
            else if index == dragIndex
              then swapSlot
              else slot) (zip [0..] slots)

getDraggedBounds :: Resources -> Box -> [Slot] -> Int -> Box
getDraggedBounds resources bounds@(Box (left, _) (right, _)) slots index =
  dragBounds
  where
    (Box (_, slotTop) _) = getSlotBounds bounds slots index
    (_, mouseY) = mousePosition resources
    (_, dragStartY) = mouseDragStartPosition resources
    dragTop = mouseY + dragOffset (slots !! index)
    dragBottom = dragTop - slotHeight
    dragBounds = Box (left, dragTop) (right, dragBottom)

incomingHeightAnimation = animateOnce $ linearRange 100 100 20
incomingAlphaAnimation = animateOnce $ linearRange 0 1 20
incomingScaleAnimation = animateOnce $ linearRange 1 1 20
outgoingHeightAnimation = animateOnce $ linearRange 100 0 20
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 20
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 20

drawSlots :: Resources -> Box -> [Slot] -> IO [(M.MiniChartOutput, Dirty)]
drawSlots resources bounds slots =
  mapM (drawOneSlot resources bounds slots) [0 .. length slots - 1]
   
drawOneSlot :: Resources -> Box -> [Slot] -> Int
    -> IO (M.MiniChartOutput, Dirty)
drawOneSlot resources bounds@(Box (left, top) (right, bottom)) slots index = 
  preservingMatrix $ do
    -- Translate to the bottom from the center
    translate $ vector3 0 (-(boxHeight bounds / 2)) 0

    -- Translate back up to the center of the slot
    translate $ vector3 0 (boxTop slotBounds - slotHeight / 2) 0

    scale3 scale scale 1
    output <- M.drawMiniChart resources input
    return (output, alphaDirty || scaleDirty || heightDirty)
  where
    slot = slots !! index
    (height, heightDirty) = current $ heightAnimation slot
    (alpha, alphaDirty) = current $ alphaAnimation slot
    (scale, scaleDirty) = current $ scaleAnimation slot

    slotDragged = isBeingDragged slot
    slotBounds = if slotDragged
        then getDraggedBounds resources bounds slots index
        else getSlotBounds bounds slots index

    input = M.MiniChartInput
      { M.bounds = slotBounds
      , M.alpha = alpha
      , M.symbol = symbol slot
      , M.isBeingDragged = slotDragged
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

