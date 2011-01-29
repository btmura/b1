module B1.Program.Chart.FtglUtils
  ( prepareLayoutText
  , renderLayoutText
  ) where
  
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL

import B1.Program.Chart.Resources

prepareLayoutText :: Resources -> Int -> GLfloat -> String -> IO [GLfloat]
prepareLayoutText (Resources { font = font, layout = layout }) fontSize
    lineLength text = do
  setFontFaceSize font fontSize 72
  setLayoutFont layout font
  setLayoutLineLength layout (realToFrac lineLength)
  [left, bottom, _, right, top, _] <- getLayoutBBox layout text
  return $ map realToFrac [left, bottom, right, top]

renderLayoutText :: Resources -> String -> IO ()
renderLayoutText Resources { layout = layout } = renderLayout layout

