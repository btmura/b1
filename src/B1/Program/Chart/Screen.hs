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
      { F.width = 0
      , F.height = 0
      , F.inputState = F.newFrameState
      } 

drawScreenLoop :: S.SideBarInput -> F.FrameInput
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarInput frameInput resources = do
  loadIdentity

  sideBarOutput <- preservingMatrix $ do
    S.drawSideBar resources sideBarInput

  frameOutput <- preservingMatrix $ do
    translate $ vector3 frameTranslateX frameTranslateY 0
    F.drawChartFrame resources revisedFrameInput

  let nextSideBarInput = sideBarInput
      nextFrameInput = frameInput { F.inputState = F.outputState frameOutput }
      nextDirty =  S.isDirty sideBarOutput || F.isDirty frameOutput
  return (Action (drawScreenLoop S.SideBarInput nextFrameInput), nextDirty)

  where
    revisedFrameInput = frameInput
      { F.width = mainFrameWidth resources
      , F.height = mainFrameHeight resources
      }

    frameTranslateX = sideBarWidth resources + mainFrameWidth resources / 2
    frameTranslateY = mainFrameHeight resources / 2

mainFrameWidth :: Resources -> GLfloat
mainFrameWidth resources = windowWidth resources - sideBarWidth resources

mainFrameHeight :: Resources -> GLfloat
mainFrameHeight = windowHeight


