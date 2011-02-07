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
      { S.newSymbol = Nothing
      , S.inputState = S.newSideBarState
      }
    F.FrameInput
      { F.bounds = zeroBox
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
        { S.newSymbol = F.addedSymbol frameOutput
        , S.inputState = S.outputState sideBarOutput
        }
      nextFrameInput = frameInput
        { F.inputState = F.outputState frameOutput
        }
      nextDirty =  S.isDirty sideBarOutput || F.isDirty frameOutput
  return (Action (drawScreenLoop nextSideBarInput nextFrameInput), nextDirty)

  where
    frameLeft = sideBarWidth resources
    frameRight = frameLeft + mainFrameWidth resources
    frameBottom = mainFrameHeight resources
    revisedFrameInput = frameInput
      { F.bounds = Box (frameLeft, 0) (frameRight, frameBottom)
      }

    frameTranslateX = sideBarWidth resources + mainFrameWidth resources / 2
    frameTranslateY = mainFrameHeight resources / 2

mainFrameWidth :: Resources -> GLfloat
mainFrameWidth resources = windowWidth resources - sideBarWidth resources

mainFrameHeight :: Resources -> GLfloat
mainFrameHeight = windowHeight
