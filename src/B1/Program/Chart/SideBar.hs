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
import B1.Data.Symbol
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.MiniChart as M

slotHeight = 100::GLfloat

dragFreelyOffset = slotHeight / 2

scrollIncrement = 50::GLfloat

dragScrollIncrement = 10::GLfloat

incomingHeightAnimation = animateOnce $ linearRange 100 100 20
incomingAlphaAnimation = animateOnce $ linearRange 0 1 20
incomingScaleAnimation = animateOnce $ linearRange 1 1 20

outgoingHeightAnimation = animateOnce $ linearRange 100 0 10
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 10
outgoingScaleAnimation = animateOnce $ linearRange 1 1.25 10

draggedInHeightAnimation = animateOnce $ linearRange 50 100 10
draggedInAlphaAnimation = animateOnce $ linearRange 1 1 20
draggedInScaleAnimation = animateOnce $ linearRange 1 1 20

draggedOutHeightAnimation = animateOnce $ linearRange 100 0 10
draggedOutAlphaAnimation = animateOnce $ linearRange 0 0 20
draggedOutScaleAnimation = animateOnce $ linearRange 0 0 20

data SideBarInput = SideBarInput
  { bounds :: Box
  , newSymbols :: [Symbol]
  , newMiniChartDraggedIn :: Maybe M.MiniChartState
  , justSelectedSymbol :: Maybe Symbol
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
  , dragFreely :: Bool
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
      , newMiniChartDraggedIn = draggedInChart
      , justSelectedSymbol = justSelectedSymbol
      , inputState = inputState
      } = do

  newSlots <- createSlots newSymbols

  (drawSlots resources bounds  
      . addDraggingScrollAmount resources bounds
      . calculateNextScrollAmount resources bounds justSelectedSymbol
      . reorderSlotsBeingDragged resources bounds
      . markSlotsBeingDragged resources bounds
      . insertDraggedInSlot resources bounds draggedInChart
      . addNewSlots newSlots) inputState

createSlots :: [Symbol] -> IO [Slot]
createSlots =
  mapM (\symbol -> do
      miniChartState <- M.newMiniChartState symbol Nothing
      return Slot
        { symbol = symbol
        , remove = False
        , isBeingDragged = False
        , dragFreely = False
        , dragOffset = 0
        , heightAnimation = incomingHeightAnimation
        , alphaAnimation = incomingAlphaAnimation
        , scaleAnimation = incomingScaleAnimation
        , miniChartState = miniChartState
        })

addNewSlots :: [Slot] -> SideBarState -> SideBarState 
addNewSlots newSlots state@SideBarState { slots = slots } =
  state
    { newSlots = newSlots
    , slots = foldl addOnlyUniqueSymbols slots newSlots
    }

addOnlyUniqueSymbols :: [Slot] -> Slot -> [Slot]
addOnlyUniqueSymbols slots newSlot
  | alreadyAdded = slots
  | otherwise = slots ++ [newSlot]
  where
    alreadyAdded = any (containsSymbol (symbol newSlot)) slots

containsSymbol :: Symbol -> Slot -> Bool
containsSymbol newSymbol slot = symbol slot == newSymbol && not (remove slot)

insertDraggedInSlot :: Resources -> Box -> Maybe M.MiniChartState
    -> SideBarState -> SideBarState

insertDraggedInSlot resources bounds (Just draggedInChart)
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      } =
  state { slots = newSlots }
  where
    (mouseX, mouseY) = mousePosition resources
    indexedSlots = zip [0 .. length slots - 1] slots

    isSlotAboveMousePosition :: (Int, Slot) -> Bool
    isSlotAboveMousePosition (index, slot) =
      let Box (_, _) (_, bottom) = getSlotBounds bounds scrollAmount slots index
      in mouseY < bottom

    (indexedSlotsAbove, indexedSlotsBelow) =
        span isSlotAboveMousePosition indexedSlots
    removeIndices = snd . unzip
    slotsAbove = removeIndices indexedSlotsAbove 
    slotsBelow = removeIndices indexedSlotsBelow
    draggedInSlot = createDraggedInSlot draggedInChart

    alreadyAdded = any (containsSymbol (M.symbol draggedInChart)) slots
    newSlots = if alreadyAdded
        then slots
        else slotsAbove ++ [draggedInSlot] ++ slotsBelow

insertDraggedInSlot _ _ Nothing state = state

createDraggedInSlot :: M.MiniChartState -> Slot
createDraggedInSlot draggedInChart =
  Slot
    { symbol = M.symbol draggedInChart
    , remove = False
    , isBeingDragged = True
    , dragFreely = True
    , dragOffset = dragFreelyOffset
    , heightAnimation = draggedInHeightAnimation
    , alphaAnimation = draggedInAlphaAnimation
    , scaleAnimation = draggedInScaleAnimation
    , miniChartState = draggedInChart
    }

calculateNextScrollAmount :: Resources -> Box -> Maybe Symbol 
    -> SideBarState -> SideBarState
calculateNextScrollAmount resources bounds justSelectedSymbol
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
    allShowing = areAllSlotsShowing bounds slots
    addedNewSlots = length newSlots > 0

    needScrollToSelected = isJust justSelectedSymbol 
        && any (containsSymbol (fromJust justSelectedSymbol)) slots
        && not (boxContainsBox bounds selectedBounds)

    selectedIndex = (fst . head 
        . filter (containsSymbol (fromJust justSelectedSymbol) . snd)) $
            zip [0..] slots
    selectedBounds = getSlotBounds bounds scrollAmount slots selectedIndex
    selectedScrollAmount 
      | boxBottom selectedBounds >= boxTop bounds =
          scrollAmount - (boxTop selectedBounds - boxTop bounds)
      | boxTop selectedBounds <= boxBottom bounds = 
          scrollAmount + (boxBottom bounds - boxBottom selectedBounds) 
      | otherwise = scrollAmount

    scrolling = isMouseWheelMoving resources
    maxScrollAmount = getMaximumScrollAmount bounds slots
    possiblySameScrollAmount = min scrollAmount maxScrollAmount
    adjustedScrollAmount =
      if getMouseWheelVelocity resources > 0 
        then getScrollUpAmount bounds slots scrollAmount scrollIncrement
        else getScrollDownAmount scrollAmount scrollIncrement

areAllSlotsShowing :: Box -> [Slot] -> Bool
areAllSlotsShowing bounds slots = visibleHeight > getTotalHeight slots
  where
    visibleHeight = boxHeight bounds

getTotalHeight :: [Slot] -> GLfloat
getTotalHeight slots = sum $ map (fst . current . heightAnimation) slots

getMaximumScrollAmount :: Box -> [Slot] -> GLfloat
getMaximumScrollAmount bounds slots =
  if areAllSlotsShowing bounds slots
    then 0
    else getTotalHeight slots - visibleHeight
  where
    visibleHeight = boxHeight bounds

getScrollUpAmount :: Box -> [Slot] -> GLfloat -> GLfloat -> GLfloat
getScrollUpAmount bounds slots scrollAmount increment =
  min (scrollAmount + increment) $ getMaximumScrollAmount bounds slots

getScrollDownAmount :: GLfloat -> GLfloat -> GLfloat
getScrollDownAmount scrollAmount increment = max (scrollAmount - increment) 0

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
  | hasMouseDragFinished resources = updatedSlot { dragFreely = False }
  | hasMouseDragStarted resources = updatedSlot
  | isMouseDrag resources
      && dragFreely slot
      && not (remove slot)
      && not (boxContains bounds (mousePosition resources)) = removeSlot
  | otherwise = slot
  where
    slot = slots !! index
    (beingDragged, dragOffset) =
        isDraggingSlot resources bounds scrollAmount slots index
    
    updatedSlot = slot
      { isBeingDragged = beingDragged
      , dragOffset = dragOffset
      }

    removeSlot = slot
      { isBeingDragged = False
      , dragOffset = 0
      , remove = True
      , heightAnimation = draggedOutHeightAnimation
      , alphaAnimation = draggedOutAlphaAnimation
      , scaleAnimation = draggedOutScaleAnimation
      }

isDraggingSlot :: Resources -> Box -> GLfloat -> [Slot] -> Int
    -> (Bool, GLfloat)
isDraggingSlot resources bounds scrollAmount slots index =
  (slotDragged, dragOffset)
  where
    slotDragged
      | not (isMouseDrag resources) = False
      | isDraggedFreely = boxContains bounds (mousePosition resources)
      | otherwise = boxContains slotBounds dragStartPosition

    isDraggedFreely = dragFreely $ slots !! index
    slotBounds = getSlotBounds bounds scrollAmount slots index
    dragStartPosition = mouseDragStartPosition resources

    dragOffset
      | not (isMouseDrag resources) = 0
      | isDraggedFreely = dragFreelyOffset
      | otherwise = boxTop slotBounds - snd dragStartPosition

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
  | null indexedDragSlots = state
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
    (mouseX, mouseY) = mousePosition resources
    (_, dragStartY) = mouseDragStartPosition resources

    draggedSlot = slots !! index
    (dragLeft, dragRight) =
        if dragFreely draggedSlot
          then
            let halfWidth = boxWidth bounds / 2
            in (mouseX - halfWidth, mouseX + halfWidth)
          else (left, right)

    dragTop = mouseY + dragOffset draggedSlot
    dragBottom = dragTop - slotHeight
    dragBounds = Box (dragLeft, dragTop) (dragRight, dragBottom)

drawSlots :: Resources -> Box -> SideBarState -> IO SideBarOutput
drawSlots resources bounds
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      } = do
  output <- mapM (drawOneSlot resources bounds scrollAmount slots)
      [0 .. length slots - 1]
  convertDrawingOutputs state output

drawOneSlot :: Resources -> Box -> GLfloat -> [Slot] -> Int
    -> IO (M.MiniChartOutput, Dirty)
drawOneSlot resources bounds@(Box (left, top) (right, bottom)) scrollAmount
    slots index = 
  preservingMatrix $ do
    -- Translate to the bottom left from the center
    translate $ vector3 (-(boxWidth bounds / 2)) (-(boxHeight bounds / 2)) 0

    -- Translate back up to the center of the slot
    translate $ vector3 (boxRight slotBounds - boxWidth slotBounds / 2)
        (boxTop slotBounds - slotHeight / 2) 0

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
      , M.isBeingDragged = slotDragged
      , M.inputState = miniChartState slot
      }

convertDrawingOutputs :: SideBarState -> [(M.MiniChartOutput, Dirty)]
    -> IO SideBarOutput
convertDrawingOutputs state output = do
  mapM_ (M.cleanMiniChartState . miniChartState) cleanSlots
  return nextOutput
  where
    (outputStates, dirtyFlags) = unzip output
    updatedSlots = zipWith updateMiniChartState (slots state) outputStates
    cleanSlots = filter
        (\slot -> shouldRemoveSlot slot && not (dragFreely slot))
        updatedSlots
    nextSlots = filter (not . shouldRemoveSlot) updatedSlots
    nextState = state { slots = nextSlots } 

    isSideBarDirty = length (newSlots state) > 0
        || any M.isDirty outputStates
        || any id dirtyFlags
    nextSymbolRequest = (listToMaybe . mapMaybe M.symbolRequest) outputStates
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

addDraggingScrollAmount :: Resources -> Box -> SideBarState -> SideBarState
addDraggingScrollAmount resources bounds 
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      }
  | dragging = state { scrollAmount = dragScrollAmount }
  | otherwise = state
  where
    dragHeight = 20::GLfloat
    dragTopBox = Box (boxLeft bounds, boxTop bounds)
        (boxRight bounds, boxTop bounds - dragHeight)
    dragBottomBox = Box (boxLeft bounds, boxBottom bounds + dragHeight)
        (boxRight bounds, boxBottom bounds)
    draggingAtTop = isMouseDrag resources
        && boxContains dragTopBox (mousePosition resources)
    draggingAtBottom = isMouseDrag resources
        && boxContains dragBottomBox (mousePosition resources)
    dragging = draggingAtTop || draggingAtBottom
    dragScrollAmount
      | draggingAtTop = getScrollDownAmount scrollAmount dragScrollIncrement
      | draggingAtBottom = getScrollUpAmount bounds slots scrollAmount
          dragScrollIncrement
      | otherwise = scrollAmount


