module B1.Program.Chart.Chart
  ( drawChart
  ) where

import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources

drawChart :: Resources -> GLfloat -> GLfloat -> String -> IO ()
drawChart resources@Resources { layout = layout } width height symbol = do
  [left, bottom, right, top] <- prepareTextLayout resources fontSize
      layoutLineLength symbol

  let textHeight = abs $ bottom - top
      textCenterX = -(width / 2) + symbolPadding
      textCenterY = height / 2 - symbolPadding - textHeight
          
  preservingMatrix $ do 
    translate $ vector3 textCenterX textCenterY 0
    renderLayout layout symbol

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    symbolPadding = 15

