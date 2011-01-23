module B1.Program.Chart.Chart
  ( drawChart
  ) where

import Control.Concurrent.MVar
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Text.Printf

import B1.Data.Price
import B1.Data.Price.Google
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.ChartFrameSpec
import B1.Program.Chart.Colors
import B1.Program.Chart.FtglUtils
import B1.Program.Chart.Resources

drawChart :: Resources -> ChartFrameSpec -> String
    -> MVar PriceErrorTuple -> IO ()
drawChart resources@Resources { layout = layout }
    (ChartFrameSpec width height alpha) symbol priceErrorTupleMVar = do
  header <- getHeader symbol priceErrorTupleMVar
  [left, bottom, right, top] <- prepareTextLayout resources fontSize
      layoutLineLength header

  let textHeight = abs $ bottom - top
      textCenterX = -(width / 2) + symbolPadding
      textCenterY = height / 2 - symbolPadding - textHeight

  color $ green alpha
  preservingMatrix $ do 
    translate $ vector3 textCenterX textCenterY 0
    renderLayout layout header

  where
    fontSize = 18
    layoutLineLength = realToFrac width
    symbolPadding = 15

getHeader :: String -> MVar PriceErrorTuple -> IO String
getHeader symbol priceErrorTupleMVar = do
  maybePriceErrorTuple <- tryTakeMVar priceErrorTupleMVar
  case maybePriceErrorTuple of
    Just priceErrorTuple -> do
      tryPutMVar priceErrorTupleMVar priceErrorTuple
      case priceErrorTuple of
        (Just (currentPrice:_), _) ->
          return $ printf "%s %0.2f" symbol (close currentPrice)
        (Nothing, (error:_)) ->
          return $ symbol ++ " [Error: " ++ error ++ "]"
        _ ->
          return $ symbol ++ " ?"
    _ -> return $ symbol

