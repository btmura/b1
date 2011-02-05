module B1.Graphics.Rendering.FTGL.Utils
  ( BoundingBox(..)
  , FontSize
  , Point
  , Text
  , TextSpec(..)
  , boxCenter
  , boxContains
  , boxWidth
  , boxHeight
  , measureText
  , renderText
  ) where
  
import Graphics.Rendering.FTGL
import qualified Graphics.Rendering.OpenGL as GL

-- TODO: Extract Point and BoundingBox functions to separate modules.
type Point = (GL.GLfloat, GL.GLfloat) 

data BoundingBox = 
  -- | Construct a box from upper left and bottom right points.  
  BoundingBox Point Point 

boxWidth :: BoundingBox -> GL.GLfloat
boxWidth (BoundingBox (left, _) (right, _)) = abs $ right - left

boxHeight :: BoundingBox -> GL.GLfloat
boxHeight (BoundingBox (_, top) (_, bottom)) = abs $ bottom - top

boxCenter :: BoundingBox -> Point
boxCenter box@(BoundingBox (left, top) _) = (centerX, centerY)
  where
    centerX = left + boxWidth box / 2
    centerY = top - boxHeight box / 2

boxContains :: BoundingBox -> Point -> Bool
boxContains box@(BoundingBox (left, top) (right, bottom)) (x, y) =
  x >= left && x <= right && y >= top && y <= bottom

dpi = 72

type FontSize = Int

type Text = String

data TextSpec = TextSpec Font FontSize Text

measureText :: TextSpec -> IO BoundingBox
measureText (TextSpec font fontSize text) = do
  oldFontSize <- getFontFaceSize font
  setFontFaceSize font fontSize dpi
  [left, bottom, _, right, top, _] <- getFontBBox font text
  setFontFaceSize font oldFontSize dpi
  return $ BoundingBox (realToFrac left, realToFrac top)
      (realToFrac right, realToFrac bottom)


renderText :: TextSpec -> IO ()
renderText (TextSpec font fontSize text) = do
  setFontFaceSize font fontSize dpi
  renderFont font text Front

