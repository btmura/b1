module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
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
    ScreenState
      { sideBarOpen = False
      , sideBarWidthAnimation = animateOnce $ linearRange 0 0 30
      }

data ScreenState = ScreenState
  { sideBarOpen :: Bool
  , sideBarWidthAnimation :: Animation (GLfloat, Dirty)
  }

sideBarOpenWidth = 150
openSideBarAnimation = animateOnce $ linearRange 0 sideBarOpenWidth 10
closeSideBarAnimation = animateOnce $ linearRange sideBarOpenWidth 0 10

drawScreenLoop :: S.SideBarInput -> F.FrameInput -> ScreenState
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop
    sideBarInput@S.SideBarInput
      { S.inputState = S.SideBarState { S.slots = slots }
      }
    frameInput
    screenState@ScreenState
      { sideBarOpen = sideBarOpen
      , sideBarWidthAnimation = sideBarWidthAnimation
      }
    resources = do

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
      nextScreenState = screenState
        { sideBarOpen = nextSideBarOpen
        , sideBarWidthAnimation = nextSideBarWidthAnimation
        }
      nextDirty = sideBarWidthDirty
          || nextSideBarWidthDirty
          || S.isDirty sideBarOutput
          || F.isDirty frameOutput
  return (Action (drawScreenLoop nextSideBarInput nextFrameInput
      nextScreenState), nextDirty)

  where
    (sideBarWidth, sideBarWidthDirty) = current sideBarWidthAnimation

    nextSideBarOpen = length slots > 0
    nextSideBarWidthAnimation
      | not sideBarOpen && nextSideBarOpen = openSideBarAnimation
      | sideBarOpen && not nextSideBarOpen = closeSideBarAnimation
      | otherwise = next sideBarWidthAnimation

    nextSideBarWidthDirty = snd . current $ nextSideBarWidthAnimation

    height = windowHeight resources

    sideBarTopPadding = 5

    sideBarBounds = Box (0, height - sideBarTopPadding) (sideBarWidth, 0)
    frameBounds = Box (sideBarWidth, height) (windowWidth resources, 0)

