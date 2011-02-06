module B1.Graphics.Rendering.FTGL.Utils
  ( FontSize
  , TextSpec(..)
  , measureText
  , renderText
  ) where
  
import Graphics.Rendering.FTGL

import B1.Graphics.Rendering.OpenGL.Box

dpi = 72

type FontSize = Int

data TextSpec = TextSpec Font FontSize String

measureText :: TextSpec -> IO Box
measureText (TextSpec font fontSize text) = do
  oldFontSize <- getFontFaceSize font
  setFontFaceSize font fontSize dpi
  [left, bottom, _, right, top, _] <- getFontBBox font text
  setFontFaceSize font oldFontSize dpi
  return $ Box (realToFrac left, realToFrac top)
      (realToFrac right, realToFrac bottom)

renderText :: TextSpec -> IO ()
renderText (TextSpec font fontSize text) = do
  setFontFaceSize font fontSize dpi
  renderFont font text Front

