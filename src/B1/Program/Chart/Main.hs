module B1.Program.Chart.Main
  ( main
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Action
import B1.Graphics.Rendering.OpenGL.Utils

main :: IO ()
main = do
  initialize
  createWindow
  drawLoop $ drawScreen drawSideBar drawMainChart
  closeWindow
  terminate

createWindow :: IO ()
createWindow = do
  openWindow (Size 400 400) [DisplayAlphaBits 8] Window
  windowTitle $= "B1"
  windowSizeCallback $= myWindowSizeCallback

myWindowSizeCallback :: Size -> IO ()
myWindowSizeCallback size@(Size width height) = do
  viewport $= (Position 0 0, size)

drawLoop :: IO Action -> IO ()
drawLoop action = do
  clear [ColorBuffer, DepthBuffer]
  Action nextAction <- action
  swapBuffers
  sleep 0.001

  esc <- getKey ESC
  case esc of
    Press -> return ()
    Release -> drawLoop nextAction

drawScreen :: IO Action -> IO Action -> IO Action
drawScreen sideBarAction mainChartAction = do
  loadIdentity

  (_, (Size width height)) <- get viewport

  let sideBarWidth = 150
      sideBarWidthPercentage = sideBarWidth / realToFrac width
      mainChartWidthPercentage = 1.0 - sideBarWidthPercentage

  translate $ vector3 (-1) 0 0
  translate $ vector3  sideBarWidthPercentage 0 0
  Action nextSideBarAction <- preservingMatrix $ do
    scale3 sideBarWidthPercentage 1 1
    sideBarAction

  translate $ vector3 sideBarWidthPercentage 0 0
  translate $ vector3  mainChartWidthPercentage 0 0
  Action nextMainChartAction <- preservingMatrix $ do
    scale3 mainChartWidthPercentage 1 1
    mainChartAction

  return $ Action (drawScreen nextSideBarAction nextMainChartAction)

drawSideBar :: IO Action
drawSideBar = do
  color $ color3 0 0 1
  drawSquare
  return $ Action drawSideBar

drawMainChart :: IO Action
drawMainChart = do
  color $ color3 1 0 0
  drawSquare

  space <- getKey ' '
  case space of
    Release -> return $ Action drawMainChart
    Press -> return $ Action (rotateMainChart 0)

rotateMainChart :: GLfloat -> IO Action
rotateMainChart rotateY = do
  color $ color3 0 1 0
  rotate rotateY $ vector3 0 1 0
  drawSquare
  if rotateY >= 180
    then return $ Action drawMainChart
    else return $ Action (rotateMainChart (rotateY + 1))

drawSquare :: IO ()
drawSquare = do
  renderPrimitive LineLoop $ do
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 1
    vertex $ vertex2 1 (-1)

  renderPrimitive Lines $ do
    vertex $ vertex2 (-1) 1
    vertex $ vertex2 1 (-1)
    vertex $ vertex2 (-1) (-1)
    vertex $ vertex2 1 1

