module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , SideBarState(..)
  , drawSideBar
  , newSideBarState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
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
  { slotBounds :: Box
  , symbol :: Symbol
  , miniChartState :: M.MiniChartState
  } deriving (Show)

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

  let nextSlotBounds = getNextSlotBounds bounds slots
      nextSlots = addSymbol maybeNewSymbol nextSlotBounds slots

  outputStates <- mapM (M.drawMiniChart resources
      . createMiniChartInput bounds) nextSlots

  let nextState = SideBarState { slots = nextSlots } 
      isSideBarDirty = isJust maybeNewSymbol || any M.isDirty outputStates
  return SideBarOutput
    { isDirty = isSideBarDirty
    , outputState = nextState
    }

getNextSlotBounds :: Box -> [Slot] -> Box
getNextSlotBounds (Box (left, top) (right, _)) slots = 
  Box (left, slotTop) (right, slotBottom)
  where
    slotTop = case slots of
      [] -> top
      _ -> (boxBottom . slotBounds . last) slots
    slotHeight = 100
    slotBottom = slotTop - slotHeight

addSymbol :: Maybe Symbol -> Box -> [Slot] -> [Slot]
addSymbol Nothing _ slots = slots
addSymbol (Just symbol) bounds slots =
  slots ++ [newSlot]
  where
    newSlot = Slot
      { slotBounds = bounds
      , symbol = symbol
      , miniChartState = M.newMiniChartState
      }

createMiniChartInput :: Box -> Slot -> M.MiniChartInput
createMiniChartInput
  (Box (left, _) (right, _))
  Slot
    { slotBounds = Box (_, top) (_, bottom)
    , symbol = symbol
    , miniChartState = miniChartState
    } =
  M.MiniChartInput
    { M.bounds = Box (left, top) (right, bottom)
    , M.symbol = symbol
    , M.inputState = miniChartState
    }

