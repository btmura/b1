module B1.Program.Chart.FtglUtils
  ( prepareTextLayout
  ) where
  
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Program.Chart.Resources

prepareTextLayout :: Resources -> Int -> Float -> String -> IO [GLfloat]
prepareTextLayout (Resources { font = font, layout = layout }) fontSize
    lineLength text = do
  setFontFaceSize font fontSize 72
  setLayoutFont layout font
  setLayoutLineLength layout (realToFrac lineLength)
  [left, bottom, _, right, top, _] <- getLayoutBBox layout text
  return $ map realToFrac [left, bottom, right, top]

