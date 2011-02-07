module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , SideBarState(..)
  , drawSideBar
  , newSideBarState
  ) where

import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources
import B1.Program.Chart.Symbol

data SideBarInput = SideBarInput
  { newSymbol :: Maybe Symbol
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
      { newSymbol = newSymbol
      , inputState = SideBarState { symbols = currentSymbols }
      } = do
  loadIdentity

  let sideBarHeight = realToFrac windowHeight
  translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth / 2) (sideBarHeight / 2) 1
  color $ color4 0 0 1 0.5
  drawSquarePlaceholder

  let nextSymbols = currentSymbols ++ catMaybes [newSymbol]
      nextState = SideBarState
        { symbols = nextSymbols
        }
  return SideBarOutput
    { isDirty = False
    , outputState = nextState
    }

