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
  loadIdentity
  translate $ vector3 (sideBarWidth + (mainChartWidth resources) / 2)
      ((mainChartHeight resources) / 2) 0

  scale3 rangeValue 1 1

  color $ color4 0.25  1 0 rangeValue
  drawCenteredInstructions resources

  color $ color4 0 0.25 1 rangeValue
  drawChart resources

  case nextRangeValues of
    [] -> return (Action (drawMainChart rangeValues), False)
    _ -> return (Action (drawMainChart nextRangeValues), True)

mainChartWidth :: Resources -> GLfloat
mainChartWidth resources = realToFrac (windowWidth resources) - sideBarWidth

mainChartHeight :: Resources -> GLfloat
mainChartHeight resources = realToFrac (windowHeight resources)

drawChart :: Resources -> IO ()
drawChart resources = do
  preservingMatrix $ do
    let padding = 10
        numRoundedVertices = 5
        roundedSize = 10
        translateX = mainChartWidth resources / 2 - padding
        translateY = mainChartHeight resources / 2 - padding

        cornerTranslateX = translateX - roundedSize
        cornerTranslateY = translateY - roundedSize

    preservingMatrix $ do
      translate $ vector3 cornerTranslateX (-cornerTranslateY) 0
      scale3 roundedSize roundedSize 1
      renderPrimitive LineStrip $ do
        mapM_ (\x -> vertex $ circleVertex2 x)
            (linearRange (2 * pi) (3 * pi / 2) numRoundedVertices) 

    renderPrimitive Lines $ do
      vertex $ vertex2 (translateX - roundedSize) (-translateY)
      vertex $ vertex2 (-translateX + roundedSize) (-translateY)

    preservingMatrix $ do
      translate $ vector3 (-cornerTranslateX) (-cornerTranslateY) 0
      scale3 roundedSize roundedSize 1
      renderPrimitive LineStrip $ do
        mapM_ (\x -> vertex $ circleVertex2 x)
            (linearRange (3 * pi / 2) pi numRoundedVertices)

    renderPrimitive Lines $ do
      vertex $ vertex2 (-translateX) (translateY - roundedSize)
      vertex $ vertex2 (-translateX) (-translateY + roundedSize)

    preservingMatrix $ do
      translate $ vector3 (-cornerTranslateX) cornerTranslateY 0
      scale3 roundedSize roundedSize 1
      renderPrimitive LineStrip $ do
        mapM_ (\x -> vertex $ circleVertex2 x)
            (linearRange pi (pi / 2) numRoundedVertices)

    renderPrimitive Lines $ do
      vertex $ vertex2 (-translateX + roundedSize) translateY
      vertex $ vertex2 (translateX - roundedSize) translateY

    preservingMatrix $ do
      translate $ vector3 cornerTranslateX cornerTranslateY 0
      scale3 roundedSize roundedSize 1
      renderPrimitive LineStrip $ do
        mapM_ (\x -> vertex $ circleVertex2 x)
            (linearRange (pi / 2) 0 numRoundedVertices) 

    renderPrimitive Lines $ do
      vertex $ vertex2 translateX (translateY - roundedSize)
      vertex $ vertex2 translateX (-(translateY - roundedSize))

drawCenteredInstructions :: Resources -> IO ()
drawCenteredInstructions resources =
  preservingMatrix $ do
    let instructions = "Type in symbol and press ENTER..."
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

