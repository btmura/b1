module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.ChartFrame as F
import qualified B1.Program.Chart.SideBar as S

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen = drawScreenLoop
    S.SideBarInput
    F.FrameInput
      { F.inputState = F.newFrameState
      } 

drawScreenLoop :: S.SideBarInput -> F.FrameInput
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarInput frameInput resources = do
  sideBarOutput <- S.drawSideBar resources sideBarInput
  frameOutput <- F.drawChartFrame resources frameInput
  let nextSideBarInput = S.SideBarInput
      nextFrameInput = F.FrameInput { F.inputState = F.outputState frameOutput }
      nextDirty =  S.isDirty sideBarOutput || F.isDirty frameOutput
  return (Action (drawScreenLoop S.SideBarInput nextFrameInput), nextDirty)

