module B1.Program.Chart.Overlay
  ( OverlayInput(..)
  , OverlayOutput(..)
  , OverlayState
  , OverlayOptions(..)
  , OverlayBoundSet(..)
  , drawOverlay
  , newOverlayState
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL

import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Point
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data OverlayInput = OverlayInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: OverlayState
  }

data OverlayOutput = OverlayOutput
  { outputState :: OverlayState
  , isDirty :: Dirty
  }

data OverlayState = OverlayState
  { options :: OverlayOptions
  , stockData :: StockData
  }

data OverlayOptions = OverlayOptions
  { boundSet :: OverlayBoundSet
  }

data OverlayBoundSet = OverlayBoundSet

newOverlayState :: OverlayOptions -> StockData -> OverlayState
newOverlayState options stockData = OverlayState
  { options = options
  , stockData = stockData
  }

drawOverlay :: Resources -> OverlayInput -> IO OverlayOutput
drawOverlay resources
    input@OverlayInput
      { bounds = bounds
      , alpha = alpha
      , inputState = inputState@OverlayState
        { stockData = stockData
        }
      } = do
  handleStockData (renderOverlay resources bounds alpha)
      (\_ -> return ()) () stockData
  return OverlayOutput
    { outputState = inputState
    , isDirty = False
    }

renderOverlay :: Resources -> Box -> GLfloat -> StockPriceData -> IO ()
renderOverlay resources bounds alpha priceData =
  when (alpha > 0 && boxContains bounds (mousePosition resources)) $ do
    renderCrosshair resources bounds alpha

renderCrosshair :: Resources -> Box -> GLfloat -> IO ()
renderCrosshair resources bounds alpha =
  preservingMatrix $ do
    translateToWindowLowerLeft bounds

    let (mouseX, mouseY) = mousePosition resources
        lineColor4 = red4
    renderPrimitive Lines $ do
      -- Vertical line
      color $ lineColor4 0 
      vertex $ vertex2 mouseX (boxBottom bounds)

      color $ lineColor4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 0
      vertex $ vertex2 mouseX (boxTop bounds)

      -- Horizontal line
      color $ lineColor4 0 
      vertex $ vertex2 (boxLeft bounds) mouseY

      color $ lineColor4 alpha
      vertex $ vertex2 mouseX mouseY
      vertex $ vertex2 mouseX mouseY

      color $ lineColor4 0
      vertex $ vertex2 (boxRight bounds) mouseY

translateToWindowLowerLeft :: Box -> IO ()
translateToWindowLowerLeft bounds =
  -- Switch to global coordinates
  let translateX = (-boxWidth bounds / 2) - boxLeft bounds
      translateY = (-boxHeight bounds / 2) - boxBottom bounds
  in translate $ vector3 translateX translateY 0

