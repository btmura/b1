module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = 
  return (Action (drawScreenLoop drawSideBar drawMainChart), True)

drawScreenLoop :: (Resources -> IO (Action Resources Dirty, Dirty))
    -> (Resources -> IO (Action Resources Dirty, Dirty))
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarAction mainChartAction input = do
  (Action nextSideBarAction, sideBarDirty) <- sideBarAction input
  (Action nextMainChartAction, mainChartDirty) <- mainChartAction input
  return (Action (drawScreenLoop nextSideBarAction nextMainChartAction),
      sideBarDirty || mainChartDirty)

sideBarWidth = 175

drawSideBar :: Resources -> IO (Action Resources Dirty, Dirty)
drawSideBar resources = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth / 2) (sideBarHeight / 2) 1
  color $ color3 0 0 1
  drawSquarePlaceholder
  return (Action drawSideBar, False)

drawMainChart :: Resources -> IO (Action Resources Dirty, Dirty)
drawMainChart resources = do
  let mainChartWidth = realToFrac (windowWidth resources) - sideBarWidth
      mainChartHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth + mainChartWidth / 2)
      (mainChartHeight / 2) 0

  preservingMatrix $ do
    color $ color3 0 1 0
    translate $ vector3 (-mainChartWidth / 2) (mainChartHeight / 2 - 24) 0
    setFontFaceSize (font resources) 24 72
    renderFont (font resources) "SPY" All

  scale3 (mainChartWidth / 2) (mainChartHeight / 2) 1
  color $ color3 1 0 0
  drawSquarePlaceholder
  return (Action drawMainChart, False)

