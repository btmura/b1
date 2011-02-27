module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.ChartFrame as F
import qualified B1.Program.Chart.SideBar as S

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen = drawScreenLoop
    S.SideBarInput
      { S.bounds = zeroBox
      , S.maybeNewSymbol = Nothing
      , S.inputState = S.newSideBarState
      }
    F.FrameInput
      { F.bounds = zeroBox
      , F.inputState = F.newFrameState
      } 

drawScreenLoop :: S.SideBarInput -> F.FrameInput
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop
    sideBarInput@S.SideBarInput
      { S.inputState = S.SideBarState { S.slots = slots }
      }
    frameInput resources = do

  sideBarOutput <- preservingMatrix $ do
    translateToCenter sideBarBounds
    S.drawSideBar resources sideBarInput { S.bounds = sideBarBounds }

  frameOutput <- preservingMatrix $ do
    translateToCenter frameBounds
    F.drawChartFrame resources frameInput { F.bounds = frameBounds }

  let nextSideBarInput = sideBarInput
        { S.maybeNewSymbol = F.addedSymbol frameOutput
        , S.inputState = S.outputState sideBarOutput
        }
      nextFrameInput = frameInput
        { F.inputState = F.outputState frameOutput
        }
      nextDirty =  S.isDirty sideBarOutput || F.isDirty frameOutput
  return (Action (drawScreenLoop nextSideBarInput nextFrameInput), nextDirty)

  where
    sideBarWidth =
      case slots of
        [] -> 0
        _ -> 150

    height = windowHeight resources

    sideBarTopPadding = 5

    sideBarBounds = Box (0, height - sideBarTopPadding) (sideBarWidth, 0)
    frameBounds = Box (sideBarWidth, height) (windowWidth resources, 0)

