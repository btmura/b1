module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , SideBarState(..)
  , drawSideBar
  , newSideBarState
  ) where

import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.Range
import B1.Data.Symbol
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.LineSegment
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.ChartFrame as F
import qualified B1.Program.Chart.Graph as G
import qualified B1.Program.Chart.Header as H

slotHeight = 100::GLfloat

dragFreelyOffset = slotHeight / 2

scrollIncrement = 50::GLfloat

dragScrollIncrement = 10::GLfloat

incomingHeightAnimation = animateOnce $ linearRange 100 100 10
outgoingHeightAnimation = animateOnce $ linearRange 100 0 10

draggedInHeightAnimation = animateOnce $ linearRange 50 100 10
draggedInAlphaAnimation = animateOnce $ linearRange 1 1 10
draggedInScaleAnimation = animateOnce $ linearRange 1 1 10

draggedOutHeightAnimation = animateOnce $ linearRange 100 0 10
draggedOutAlphaAnimation = animateOnce $ linearRange 0 0 10
draggedOutScaleAnimation = animateOnce $ linearRange 0 0 10

data SideBarInput = SideBarInput
  { bounds :: Box
  , newSymbols :: [Symbol]
  , selectedSymbol :: Maybe Symbol
  , draggedSymbol :: Maybe Symbol
  , refreshRequested :: Bool
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
  , slots :: [Slot] -- ^ Slots currently part of the side bar
  , newSlots :: [Slot] -- ^ TODO: Remove this!
  , maybeFreeDragSlot :: Maybe Slot -- ^ Slot dragged outside of the side bar
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
  , frameState :: F.FrameState
  } 

newSideBarState :: SideBarState
newSideBarState  = SideBarState
  { scrollAmount = 0
  , slots = []
  , newSlots = []
  , maybeFreeDragSlot = Nothing
  }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar resources
    SideBarInput
      { bounds = bounds
      , newSymbols = newSymbols
      , selectedSymbol = selectedSymbol
      , draggedSymbol = maybeDraggedSymbol
      , refreshRequested = refreshRequested
      , inputState = inputState
      } = do
  newSlots <- createSlots newSymbols
  maybeNewFreeDragSlot <- createDraggedSlot maybeDraggedSymbol
  (drawSlots resources bounds refreshRequested 
      . addDraggingScrollAmount resources bounds
      . calculateNextScrollAmount resources bounds selectedSymbol
      . reorderSlotsBeingDragged resources bounds
      . markSlotsBeingDragged resources bounds
      . excludeFreeDragSlot resources bounds
      . includeFreeDragSlot resources bounds
      . addNewSlots newSlots maybeNewFreeDragSlot) inputState

createSlots :: [Symbol] -> IO [Slot]
createSlots = do
  let options = C.ChartOptions
        { C.headerOptions = H.HeaderOptions
          { H.fontSize = 10
          , H.padding = 5
          , H.statusStyle = H.ShortStatus
          , H.button = H.RemoveButton
          }
        , C.graphOptions = G.GraphOptions
          { G.boundSet = G.GraphBoundSet
            { G.graphBounds = Nothing
            , G.volumeBounds = Nothing
            , G.stochasticsBounds = Just $ Box (-1, 1) (1, 0)
            , G.weeklyStochasticsBounds = Just $ Box (-1, 0) (1, -1)
            , G.dividerLines =
              [ LineSegment (-1, 0) (1, -0)
              ]
            }
          , G.fontSize = 10
          }
        , C.showRefreshButton = False
        }
  mapM (\symbol -> do
      frameState <- F.newFrameState options $ Just symbol
      return Slot
        { symbol = symbol
        , remove = False
        , isBeingDragged = False
        , dragFreely = False
        , dragOffset = 0
        , heightAnimation = incomingHeightAnimation
        , alphaAnimation = incomingAlphaAnimation
        , scaleAnimation = incomingScaleAnimation
        , frameState = frameState
        })

createDraggedSlot :: Maybe Symbol -> IO (Maybe Slot)
createDraggedSlot Nothing = return Nothing
createDraggedSlot (Just symbol) = do
  newSlots <- createSlots [symbol]
  return $ (Just . head . map makeDraggedSlot) newSlots
  where
    makeDraggedSlot :: Slot -> Slot
    makeDraggedSlot slot = slot
      { isBeingDragged = True
      , dragFreely = True
      , dragOffset = dragFreelyOffset
      , heightAnimation = draggedInHeightAnimation
      , alphaAnimation = draggedInAlphaAnimation
      , scaleAnimation = draggedInScaleAnimation
      }

addNewSlots :: [Slot] -> Maybe Slot -> SideBarState -> SideBarState 
addNewSlots newSlots maybeNewFreeDragSlot
    state@SideBarState
      { slots = slots
      , maybeFreeDragSlot = maybeFreeDragSlot
      }
  | isJust maybeFreeDragSlot && isJust maybeNewFreeDragSlot =
      error "Can't drag two slots at once!"
  | otherwise = state
    { newSlots = newSlots
    , slots = foldl addOnlyUniqueSymbols slots newSlots
    , maybeFreeDragSlot =
          if isJust maybeFreeDragSlot
            then maybeFreeDragSlot
            else maybeNewFreeDragSlot
    }

addOnlyUniqueSymbols :: [Slot] -> Slot -> [Slot]
addOnlyUniqueSymbols slots newSlot
  | alreadyAdded = slots
  | otherwise = slots ++ [newSlot]
  where
    alreadyAdded = any (containsSymbol (symbol newSlot)) slots

containsSymbol :: Symbol -> Slot -> Bool
containsSymbol newSymbol slot = symbol slot == newSymbol && not (remove slot)

includeFreeDragSlot :: Resources -> Box -> SideBarState -> SideBarState
includeFreeDragSlot resources bounds
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      , maybeFreeDragSlot = maybeFreeDragSlot
      }
  | isDraggedIntoBounds = draggedIntoBoundsState
  | otherwise = state
  where
    dragSymbol = symbol $ fromJust maybeFreeDragSlot
    alreadyAdded = any (containsSymbol dragSymbol) slots
    isDraggedIntoBounds = isJust maybeFreeDragSlot
        && not alreadyAdded
        && boxContains bounds (mousePosition resources)
    draggedIntoBoundsState = state
      { slots = newSlots
      , maybeFreeDragSlot = Nothing
      }

    (mouseX, mouseY) = mousePosition resources
    isSlotAboveMousePosition :: (Int, Slot) -> Bool
    isSlotAboveMousePosition (index, slot) =
      let Box (_, _) (_, bottom) = getSlotBounds bounds scrollAmount slots index
      in mouseY < bottom

    indexedSlots = zip [0 .. ] slots
    (indexedSlotsAbove, indexedSlotsBelow) =
        span isSlotAboveMousePosition indexedSlots
    removeIndices = snd . unzip
    slotsAbove = removeIndices indexedSlotsAbove 
    slotsBelow = removeIndices indexedSlotsBelow
    newSlots = slotsAbove ++ [fromJust maybeFreeDragSlot] ++ slotsBelow

excludeFreeDragSlot :: Resources -> Box -> SideBarState -> SideBarState
excludeFreeDragSlot resources bounds
    state@SideBarState
      { slots = slots
      }
  | isDraggedOutOfBounds = draggedOutOfBoundsState
  | otherwise = state
  where
    freeDragSlots = filter dragFreely slots
    isDraggedOutOfBounds = not (null freeDragSlots)
        && not (boxContains bounds (mousePosition resources))
    withoutFreeDragSlot = filter (not . dragFreely) slots
    draggedOutOfBoundsState = state
      { slots = withoutFreeDragSlot
      , maybeFreeDragSlot = Just $ head freeDragSlots
      }

calculateNextScrollAmount :: Resources -> Box -> Maybe Symbol 
    -> SideBarState -> SideBarState
calculateNextScrollAmount resources bounds selectedSymbol
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      , newSlots = newSlots
      }
  | allShowing = state { scrollAmount = 0 }
  | addedNewSlots = state { scrollAmount = maxScrollAmount }
  | needScrollToSelected = state { scrollAmount = selectedScrollAmount }
  | scrolling = state { scrollAmount = adjustedScrollAmount }
  | otherwise = state { scrollAmount = possiblySameScrollAmount }
  where
    allShowing = areAllSlotsShowing bounds slots
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
    isDraggedFreely = dragFreely $ slots !! index
    slotDragged = isMouseDrag resources
        && (isDraggedFreely || boxContains slotBounds dragStartPosition)
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
        dragSlot
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

getDraggedBounds :: Resources -> Box -> GLfloat -> Slot -> Box
getDraggedBounds resources bounds@(Box (left, _) (right, _)) scrollAmount slot =
  dragBounds
  where
    (mouseX, mouseY) = mousePosition resources
    (_, dragStartY) = mouseDragStartPosition resources

    (dragLeft, dragRight) =
        if dragFreely slot
          then
            let halfWidth = boxWidth bounds / 2
            in (mouseX - halfWidth, mouseX + halfWidth)
          else (left, right)

    dragTop = mouseY + dragOffset slot
    dragBottom = dragTop - slotHeight
    dragBounds = Box (dragLeft, dragTop) (dragRight, dragBottom)

drawSlots :: Resources -> Box -> Bool -> SideBarState -> IO SideBarOutput
drawSlots resources bounds refreshRequested
    state@SideBarState
      { scrollAmount = scrollAmount
      , slots = slots
      , maybeFreeDragSlot = maybeFreeDragSlot
      } = do
  let drawOne = drawOneSlot resources bounds scrollAmount refreshRequested
  outputs <- mapM (drawOne slots) [0 .. length slots - 1]
  case maybeFreeDragSlot of
    Just freeDragSlot -> do
      freeDragOutput <- drawOne [freeDragSlot] 0
      convertDrawingOutputs resources state outputs [freeDragOutput]
    _ ->
      convertDrawingOutputs resources state outputs []

drawOneSlot :: Resources -> Box -> GLfloat -> Bool -> [Slot] -> Int
    -> IO (F.FrameOutput, Dirty)
drawOneSlot resources
    bounds@(Box (left, top) (right, bottom))
    scrollAmount refreshRequested slots index = 
  preservingMatrix $ do
    -- Translate to the bottom left from the center
    translate $ vector3 (-(boxWidth bounds / 2)) (-(boxHeight bounds / 2)) 0

    -- Translate back up to the center of the slot
    translate $ vector3 (boxRight slotBounds - boxWidth slotBounds / 2)
        (boxTop slotBounds - slotHeight / 2) 0

    scale3 scale scale 1
    output <- F.drawChartFrame resources input
    return (output, alphaDirty || scaleDirty || heightDirty)
  where
    slot = slots !! index
    (height, heightDirty) = current $ heightAnimation slot
    (alpha, alphaDirty) = current $ alphaAnimation slot
    (scale, scaleDirty) = current $ scaleAnimation slot

    slotBounds =
        if isBeingDragged slot
          then getDraggedBounds resources bounds scrollAmount slot
          else getSlotBounds bounds scrollAmount slots index

    maybeSymbolRequest =
        if refreshRequested 
            then Just $ symbol slot
            else Nothing

    input = F.FrameInput
      { F.bounds = slotBounds
      , F.alpha = alpha
      , F.maybeSymbolRequest = maybeSymbolRequest
      , F.inputState = frameState slot
      }

convertDrawingOutputs :: Resources -> SideBarState -> [(F.FrameOutput, Dirty)]
    -> [(F.FrameOutput, Dirty)] -> IO SideBarOutput
convertDrawingOutputs resources state outputs freeDragOutputs = do
  mapM_ (F.cleanFrameState . frameState) (cleanSlots ++ cleanFreeDragSlots)
  return nextOutput
  where
    (outputStates, dirtyFlags) = unzip outputs
    (updatedSlots, animationsDirty) = unzip $ zipWith
        (updateSlot resources)
        (slots state)
        outputStates
    cleanSlots = filter
        (\slot -> shouldRemoveSlot slot && not (dragFreely slot))
        updatedSlots
    nextSlots = filter (not . shouldRemoveSlot) updatedSlots

    (freeDragOutputStates, freeDragDirtyFlags) = unzip freeDragOutputs
    (updatedFreeDragSlots, freeDragAnimationsDirty) = unzip $ zipWith
        (updateSlot resources)
        (maybeToList (maybeFreeDragSlot state))
        freeDragOutputStates
    cleanFreeDragSlots = filter
        shouldRemoveSlot
        updatedFreeDragSlots
    nextMaybeFreeDragSlot = listToMaybe $
        filter (not . shouldRemoveSlot) updatedFreeDragSlots

    nextState = state
      { slots = nextSlots
      , maybeFreeDragSlot = nextMaybeFreeDragSlot
      } 

    isSideBarDirty = length (newSlots state) > 0
        || any F.isDirty outputStates
        || any F.isDirty freeDragOutputStates
        || any id dirtyFlags
        || any id freeDragDirtyFlags
        || any id animationsDirty
        || any id freeDragAnimationsDirty
    nextSymbolRequest = (listToMaybe . mapMaybe F.otherClickedSymbol) outputStates
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

updateSlot :: Resources -> Slot -> F.FrameOutput -> (Slot, Dirty)
updateSlot resources
    slot@Slot
      { remove = alreadyRemoved
      , dragFreely = dragFreely
      , heightAnimation = heightAnimation
      , alphaAnimation = alphaAnimation
      , scaleAnimation = scaleAnimation
      }
    F.FrameOutput
      { F.outputState = nextState
      , F.buttonClickedSymbol = buttonClickedSymbol
      } = (updatedSlot, animationsDirty)
  where
    removeNow = isJust buttonClickedSymbol
        || (dragFreely && hasMouseDragFinished resources)
    (nextHeightAnimation, nextAlphaAnimation, nextScaleAnimation) =
        if removeNow
          then (outgoingHeightAnimation, outgoingAlphaAnimation,
              outgoingScaleAnimation)
          else (next heightAnimation, next alphaAnimation,
              next scaleAnimation)

    updatedSlot = slot
      { remove = alreadyRemoved || removeNow
      , heightAnimation = nextHeightAnimation
      , alphaAnimation = nextAlphaAnimation
      , scaleAnimation = nextScaleAnimation
      , frameState = nextState
      }

    isDirty :: Animation (GLfloat, Dirty) -> Bool
    isDirty = snd . current
    animationsDirty = any isDirty [nextHeightAnimation, nextAlphaAnimation,
        nextScaleAnimation]

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


