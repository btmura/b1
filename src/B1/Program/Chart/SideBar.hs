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
  { height :: GLfloat
  , symbol :: Symbol
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

  let updatedSlots = addSymbol maybeNewSymbol slots
      indices = [0 .. length updatedSlots - 1]

  outputStates <- mapM (drawSlot resources bounds updatedSlots) indices

  let nextSlots = map (uncurry updateMiniChartState) 
          (zip updatedSlots outputStates)
      nextState = SideBarState { slots = nextSlots } 
      isSideBarDirty = isJust maybeNewSymbol || any M.isDirty outputStates
  return SideBarOutput
    { isDirty = isSideBarDirty
    , outputState = nextState
    }

addSymbol :: Maybe Symbol -> [Slot] -> [Slot]
addSymbol Nothing slots = slots
addSymbol (Just newSymbol) slots
  | alreadyAdded = slots
  | otherwise = slots ++ [newSlot]
  where
    alreadyAdded = any ((== newSymbol) . symbol) slots
    newSlot = Slot
      { height = 100
      , symbol = newSymbol
      , miniChartState = M.newMiniChartState
      }

drawSlot :: Resources -> Box -> [Slot] -> Int -> IO M.MiniChartOutput
drawSlot resources (Box (left, top) (right, bottom)) slots index =
  M.drawMiniChart resources input
  where
    slotsAbove = take index slots
    heightAbove = sum $ map height slotsAbove
    slot = slots !! index
    slotHeight = height slot
    slotTop = top - heightAbove
    slotBottom = slotTop - slotHeight
    slotBounds = Box (left, slotTop) (right, slotBottom)
    input = M.MiniChartInput
      { M.bounds = slotBounds
      , M.symbol = symbol slot
      , M.inputState = miniChartState slot
      }

updateMiniChartState :: Slot -> M.MiniChartOutput -> Slot
updateMiniChartState slot
    M.MiniChartOutput { M.outputState = nextState } =
  slot { miniChartState = nextState }

