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

slotHeight = 100::GLfloat

scrollIncrement = 50::GLfloat

incomingHeightAnimation = animateOnce $ linearRange 100 100 20
incomingAlphaAnimation = animateOnce $ linearRange 0 1 20
incomingScaleAnimation = animateOnce $ linearRange 1 1 20

outgoingHeightAnimation = animateOnce $ linearRange 100 0 10
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 10
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 10

data SideBarInput = SideBarInput
  { bounds :: Box
  , newSymbols :: [Symbol]
  , selectedSymbol :: Maybe Symbol
  , inputState :: SideBarState
  }

data SideBarOutput = SideBarOutput
  { isDirty :: Dirty
  , symbolRequest :: Maybe Symbol
  , symbols :: [Symbol]
  , outputState :: SideBarState
  }

data SideBarState = SideBarState
  { scrollAmount :: GLfloat
  , slots :: [Slot]
  , newSlots :: [Slot]
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
  { scrollAmount = 0
  , slots = []
  , newSlots = []
  }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar resources
    SideBarInput
      { bounds = bounds
      , newSymbols = newSymbols
      , selectedSymbol = selectedSymbol
      , inputState = inputState
      } = do

  newSlots <- createSlots newSymbols

  (drawSlots resources bounds  
      . reorderSlotsBeingDragged resources bounds
      . markSlotsBeingDragged resources bounds
      . calculateNextScrollAmount resources bounds selectedSymbol
      . addNewSlots) $ inputState { newSlots = newSlots }

createSlots :: [Symbol] -> IO [Slot]
createSlots symbols = do
  mapM (\symbol -> do
      miniChartState <- M.newMiniChartState symbol
      return $ Slot
        { symbol = symbol
        , remove = False
        , isBeingDragged = False
        , dragOffset = 0
        , heightAnimation = incomingHeightAnimation
        , alphaAnimation = incomingAlphaAnimation
        , scaleAnimation = incomingScaleAnimation
        , miniChartState = miniChartState
        }) symbols

addNewSlots :: SideBarState -> SideBarState 
addNewSlots state@SideBarState
    { slots = slots
    , newSlots = newSlots
    } =
  state { slots = foldl addOnlyUniqueSymbols slots newSlots }

addOnlyUniqueSymbols :: [Slot] -> Slot -> [Slot]
addOnlyUniqueSymbols slots newSlot
  | alreadyAdded = slots
  | otherwise = slots ++ [newSlot]
  where
    alreadyAdded = any (containsSymbol (symbol newSlot)) slots

containsSymbol :: Symbol -> Slot -> Bool
containsSymbol newSymbol slot = (symbol slot) == newSymbol && not (remove slot)

calculateNextScrollAmount :: Resources -> Box -> Maybe Symbol 
    -> SideBarState -> SideBarState
calculateNextScrollAmount resources bounds selectedSymbol
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      , newSlots = newSlots
      }
  | allShowing = state { scrollAmount = 0 }
  | needScrollToSelected = state { scrollAmount = selectedScrollAmount }
  | scrolling = state { scrollAmount = adjustedScrollAmount }
  | otherwise = state { scrollAmount = possiblySameScrollAmount }
  where
    visibleHeight = boxHeight bounds
    totalHeight = sum $ map (fst . current . heightAnimation) slots
    allShowing = visibleHeight > totalHeight
    addedNewSlots = length newSlots > 0

    needScrollToSelected = isJust selectedSymbol 
        && any (containsSymbol (fromJust selectedSymbol)) slots
        && not (boxContainsBox bounds selectedBounds)

    selectedIndex = (fst . head 
        . filter (containsSymbol (fromJust selectedSymbol) . snd)) $
            zip [0..] slots
    selectedBounds = getSlotBounds bounds scrollAmount slots selectedIndex
    selectedScrollAmount 
      | boxBottom selectedBounds >= boxTop bounds =
          scrollAmount - (boxTop selectedBounds - boxTop bounds)
      | boxTop selectedBounds <= boxBottom bounds = 
          scrollAmount + (boxBottom bounds - boxBottom selectedBounds) 
      | otherwise = scrollAmount

    scrolling = isMouseWheelMoving resources
    maxScrollAmount = if allShowing then 0 else totalHeight - visibleHeight
    possiblySameScrollAmount = min scrollAmount maxScrollAmount
    adjustedScrollAmount =
        if getMouseWheelVelocity resources > 0
          then min (scrollAmount + scrollIncrement) maxScrollAmount
          else max (scrollAmount - scrollIncrement) 0

markSlotsBeingDragged :: Resources -> Box -> SideBarState -> SideBarState
markSlotsBeingDragged resources bounds
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      } =
  state { slots = markedSlots }
  where
    markedSlots = map (updateIsBeingDragged resources bounds scrollAmount slots)
        [0 .. length slots - 1] 

updateIsBeingDragged :: Resources -> Box -> GLfloat -> [Slot] -> Int -> Slot
updateIsBeingDragged resources bounds scrollAmount slots index
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
    (beingDragged, dragOffset) = isDraggingSlot resources bounds scrollAmount
        slots index

isDraggingSlot :: Resources -> Box -> GLfloat -> [Slot] -> Int
    -> (Bool, GLfloat)
isDraggingSlot resources bounds scrollAmount slots index =
  (slotDragged, dragOffset)
  where
    slotBounds = getSlotBounds bounds scrollAmount slots index
    dragStartPosition = mouseDragStartPosition resources
    slotDragged = isMouseDrag resources
        && boxContains slotBounds dragStartPosition
    dragOffset = boxTop slotBounds - snd dragStartPosition

getSlotBounds :: Box -> GLfloat -> [Slot] -> Int -> Box
getSlotBounds (Box (left, top) (right, _)) scrollAmount slots index =
  slotBounds
  where
    slotsAbove = take index slots
    heightAbove = sum $ map (fst . current . heightAnimation) slotsAbove

    slot = slots !! index
    slotTop = top - heightAbove + scrollAmount
    slotBottom = slotTop - slotHeight
    slotBounds = Box (left, slotTop) (right, slotBottom)

reorderSlotsBeingDragged :: Resources -> Box -> SideBarState -> SideBarState
reorderSlotsBeingDragged resources bounds
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      }
  | length slots == 1 = state
  | length indexedDragSlots == 0 = state
  | isMouseDrag resources = state { slots = reorderedSlots }
  | otherwise = state
  where
    indexedSlots = zip [0..] slots
    indexedDragSlots = filter (isBeingDragged . snd) indexedSlots
    (dragIndex, dragSlot) = head indexedDragSlots
    dragPoint = boxCenter $ getDraggedBounds resources bounds scrollAmount
        slots dragIndex
    reorderedSlots = swapSlots resources bounds scrollAmount slots
        dragPoint dragIndex

swapSlots :: Resources -> Box -> GLfloat -> [Slot] -> Point -> Int -> [Slot]
swapSlots resources bounds scrollAmount slots dragPoint dragIndex = swappedSlots
  where
    allSlotBounds = map (getSlotBounds bounds scrollAmount slots)
        [0 .. length slots - 1] 
    indexedSlotBounds = zip [0..] allSlotBounds
    containingSlots = filter (\(_, slotBounds) ->
        boxContains slotBounds dragPoint) indexedSlotBounds

    (_, dragY) = dragPoint
    swapIndex = case containingSlots of
        ((firstIndex, _):_) -> firstIndex
        _ -> if dragY > boxTop bounds
            then 0
            else length slots - 1

    swapSlot = slots !! swapIndex
    dragSlot = slots !! dragIndex
    swappedSlots =
      map (\(index, slot) ->
          if index == swapIndex
            then dragSlot
            else if index == dragIndex
              then swapSlot
              else slot) (zip [0..] slots)

getDraggedBounds :: Resources -> Box -> GLfloat -> [Slot] -> Int -> Box
getDraggedBounds resources bounds@(Box (left, _) (right, _)) scrollAmount
    slots index = dragBounds
  where
    (Box (_, slotTop) _) = getSlotBounds bounds scrollAmount slots index
    (_, mouseY) = mousePosition resources
    (_, dragStartY) = mouseDragStartPosition resources
    dragTop = mouseY + dragOffset (slots !! index)
    dragBottom = dragTop - slotHeight
    dragBounds = Box (left, dragTop) (right, dragBottom)

drawSlots :: Resources -> Box -> SideBarState -> IO SideBarOutput
drawSlots resources bounds
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      } = do
  output <- mapM (drawOneSlot resources bounds scrollAmount slots)
      [0 .. length slots - 1]
  return $ convertDrawingOutputs state output

drawOneSlot :: Resources -> Box -> GLfloat -> [Slot] -> Int
    -> IO (M.MiniChartOutput, Dirty)
drawOneSlot resources bounds@(Box (left, top) (right, bottom)) scrollAmount
    slots index = 
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
        then getDraggedBounds resources bounds scrollAmount slots index
        else getSlotBounds bounds scrollAmount slots index

    input = M.MiniChartInput
      { M.bounds = slotBounds
      , M.alpha = alpha
      , M.symbol = symbol slot
      , M.isBeingDragged = slotDragged
      , M.inputState = miniChartState slot
      }

convertDrawingOutputs :: SideBarState -> [(M.MiniChartOutput, Dirty)]
    -> SideBarOutput
convertDrawingOutputs state output = nextOutput
  where
    (outputStates, dirtyFlags) = unzip output
    nextSlots = filter (not . shouldRemoveSlot) $
          map (uncurry updateMiniChartState) (zip (slots state) outputStates)
    nextState = state { slots = nextSlots } 

    isSideBarDirty = length (newSlots state) > 0
        || any M.isDirty outputStates
        || any id dirtyFlags
    nextSymbolRequest = (listToMaybe
        . catMaybes
        . map M.symbolRequest) outputStates
    nextSymbols = map symbol nextSlots
    nextOutput = SideBarOutput
      { isDirty = isSideBarDirty
      , symbolRequest = nextSymbolRequest
      , symbols = nextSymbols
      , outputState = nextState
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


