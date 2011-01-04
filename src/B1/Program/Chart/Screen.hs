module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Data.Action
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = 
  return (Action (drawScreenLoop drawSideBar
      (drawMainChart (gradualRange 0 1 100))), True)

drawScreenLoop :: (Resources -> IO (Action Resources Dirty, Dirty))
    -> (Resources -> IO (Action Resources Dirty, Dirty))
    -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop sideBarAction mainChartAction input = do
  (Action nextSideBarAction, sideBarDirty) <- sideBarAction input
  (Action nextMainChartAction, mainChartDirty) <- mainChartAction input
  return (Action (drawScreenLoop nextSideBarAction nextMainChartAction),
      sideBarDirty || mainChartDirty)

sideBarWidth = 175

padding = 20

drawSideBar :: Resources -> IO (Action Resources Dirty, Dirty)
drawSideBar resources = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth / 2) (sideBarHeight / 2) 1
  color $ color3 0 0 1
  drawSquarePlaceholder
  return (Action drawSideBar, False)

drawMainChart :: [GLfloat] -> Resources -> IO (Action Resources Dirty, Dirty)
drawMainChart rangeValues@(rangeValue:nextRangeValues) resources = do
  let mainChartWidth = realToFrac (windowWidth resources) - sideBarWidth
      mainChartHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth + mainChartWidth / 2)
      (mainChartHeight / 2) 0

  scale3 rangeValue 1 1

  color $ color4 0.25  1 0 rangeValue
  drawCenteredInstructions resources

  color $ color4 0 0.25 1 rangeValue
  scale3 ((mainChartWidth - padding) / 2) ((mainChartHeight - padding) / 2) 1
  drawSquarePlaceholder

  case nextRangeValues of
    [] -> return (Action (drawMainChart rangeValues), False)
    _ -> return (Action (drawMainChart nextRangeValues), True)

drawCenteredInstructions :: Resources -> IO ()
drawCenteredInstructions resources =
  preservingMatrix $ do
    let instructions = "Enter a symbol and press ENTER..."
        fontSize = 18::Int
    setFontFaceSize (font resources) fontSize 72
    [left, bottom, _, right, top, _] <- getFontBBox
        (font resources) instructions

    let textWidth = right - left
        textHeight = top - bottom
        centerX = -(realToFrac textWidth / 2)
        centerY = -(realToFrac textHeight / 2)
    translate $ vector3 centerX centerY 0
    renderFont (font resources) instructions All


