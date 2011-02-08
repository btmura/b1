module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , SideBarState(..)
  , drawSideBar
  , newSideBarState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

data SideBarInput = SideBarInput
  { bounds :: Box
  , newSymbol :: Maybe Symbol
  , inputState :: SideBarState
  }

data SideBarOutput = SideBarOutput
  { outputState :: SideBarState
  , isDirty :: Dirty
  }

data SideBarState = SideBarState
  { symbols :: [Symbol]
  }

newSideBarState :: SideBarState
newSideBarState  = SideBarState { symbols = [] }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar
    Resources 
      { windowHeight = windowHeight
      , sideBarWidth = sideBarWidth
      }
    SideBarInput
      { bounds = bounds
      , newSymbol = newSymbol
      , inputState = SideBarState { symbols = currentSymbols }
      } = do
  preservingMatrix $ do
    loadIdentity
    translate $ vector3 (boxWidth bounds / 2 + padding / 2)
        (boxHeight bounds - padding - summaryHeight / 2) 0
    color $ blue 1
    drawRoundedRectangle summaryWidth summaryHeight cornerRadius cornerVertices

  let nextSymbols = currentSymbols ++ catMaybes [newSymbol]
      nextState = SideBarState
        { symbols = nextSymbols
        }
  return SideBarOutput
    { isDirty = False
    , outputState = nextState
    }
  where
    padding = 5
    summaryWidth = boxWidth bounds - padding * 2
    summaryHeight = 100
    cornerRadius = 10
    cornerVertices = 5

