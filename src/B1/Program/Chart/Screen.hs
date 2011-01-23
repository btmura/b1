module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.ChartFrame
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = 
  return (Action (drawScreenLoop drawSideBar drawChartFrame), True)

drawScreenLoop :: (Resources -> IO (Action Resources Dirty, Dirty))
    -> (Resources -> IO (Action Resources Dirty, Dirty))
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarAction mainChartAction input = do
  (Action nextSideBarAction, sideBarDirty) <- sideBarAction input
  (Action nextMainChartAction, mainChartDirty) <- mainChartAction input
  return (Action (drawScreenLoop nextSideBarAction nextMainChartAction),
      sideBarDirty || mainChartDirty)

drawSideBar :: Resources -> IO (Action Resources Dirty, Dirty)
drawSideBar resources = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth resources / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth resources / 2) (sideBarHeight / 2) 1
  color $ color4 0 0 1 0.5
  drawSquarePlaceholder
  return (Action drawSideBar, False)

