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
  { symbols :: [Symbol]
  }

newSideBarState :: SideBarState
newSideBarState  = SideBarState { symbols = [] }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar
    Resources 
      { font = font
      , windowHeight = windowHeight
      }
    SideBarInput
      { bounds = bounds
      , maybeNewSymbol = maybeNewSymbol
      , inputState = SideBarState { symbols = currentSymbols }
      } = do

  let newSymbols = refreshSymbols currentSymbols maybeNewSymbol

  preservingMatrix $ do
    loadIdentity
    translate $ vector3 (boxWidth bounds / 2 + padding / 2)
        (boxHeight bounds - topPadding - summaryHeight / 2) 0

    mapM_ (\symbol -> do
      color $ green 1
      let textSpec = TextSpec font 10 symbol
      textBounds <- measureText textSpec
      preservingMatrix $ do
        translate $ vector3 (-summaryWidth / 2 + headerPadding)
          (summaryHeight / 2 - boxHeight textBounds - headerPadding) 0
        renderText textSpec

      color $ blue 1
      drawRoundedRectangle summaryWidth summaryHeight cornerRadius
          cornerVertices

      translate $ vector3 0 (-summaryHeight - padding) 0
      ) newSymbols

  let nextState = SideBarState
        { symbols = newSymbols
        }
  return SideBarOutput
    { isDirty = isJust maybeNewSymbol
    , outputState = nextState
    }
  where
    topPadding = 10
    padding = 5
    headerPadding = 5
    summaryWidth = boxWidth bounds - padding * 2
    summaryHeight = 100
    cornerRadius = 5
    cornerVertices = 5

refreshSymbols :: [Symbol] -> Maybe Symbol -> [Symbol]
refreshSymbols currentSymbols maybeSymbol
  | alreadyAdded = currentSymbols
  | otherwise = currentSymbols ++ catMaybes [maybeSymbol]
  where
    alreadyAdded =
      case maybeSymbol of
        Just symbol -> any (== symbol) currentSymbols
        _ -> False


