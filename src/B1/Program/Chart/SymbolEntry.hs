module B1.Program.Chart.SymbolEntry
  ( SymbolEntryInput(..)
  , SymbolEntryOutput(..)
  , SymbolEntryState
  , drawSymbolEntry
  , newSymbolEntryState
  ) where

import Control.Monad
import Data.Char
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Data.Symbol
import B1.Graphics.Rendering.FTGL.Utils
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data SymbolEntryState = SymbolEntryState
  { pendingSymbol :: Symbol
  }

newSymbolEntryState :: SymbolEntryState
newSymbolEntryState = SymbolEntryState { pendingSymbol = "" }

data SymbolEntryInput = SymbolEntryInput
  { bounds :: Box
  , inputState :: SymbolEntryState
  }

data SymbolEntryOutput = SymbolEntryOutput
  { outputState :: SymbolEntryState
  , isDirty :: Dirty
  , maybeEnteredSymbol :: Maybe Symbol
  }

drawSymbolEntry :: Resources -> SymbolEntryInput -> IO SymbolEntryOutput
drawSymbolEntry resources input = do
  renderSymbolEntry resources input
  return output
  where
    currentSymbol = pendingSymbol (inputState input)
    checkKeyPress = isKeyPressed resources
    maybeLetterKey = getKeyPressed resources $ map CharKey ['A'..'Z']
    output
      | checkKeyPress (SpecialKey ENTER) = handleEnterKey currentSymbol
      | checkKeyPress (SpecialKey BACKSPACE) = handleBackspaceKey currentSymbol
      | checkKeyPress (SpecialKey ESC) = handleEscapeKey currentSymbol
      | otherwise = handleCharKey maybeLetterKey currentSymbol

renderSymbolEntry :: Resources -> SymbolEntryInput -> IO ()
renderSymbolEntry resources input = do
  let symbol = pendingSymbol (inputState input)
  unless (null symbol) $ do
    let textSpec = TextSpec (font resources) 48 symbol
    textBounds <- measureText textSpec

    let textBubblePadding = 15
        textBubbleWidth = boxWidth textBounds + textBubblePadding * 2
        textBubbleHeight = boxHeight textBounds + textBubblePadding * 2
    -- Disable blending or else the background won't work.
    blend $= Disabled
    color $ black4 1
    fillRectangle textBubbleWidth textBubbleHeight textBubblePadding
    color $ blue4 1
    drawRectangle textBubbleWidth textBubbleHeight textBubblePadding
    blend $= Enabled

    let textWidth = boxWidth textBounds
        textHeight = boxHeight textBounds
    preservingMatrix $ do
      color $ green4 1
      translate $ vector3 (-textWidth / 2) (-textHeight / 2) 0
      renderText textSpec

handleEnterKey :: Symbol -> SymbolEntryOutput
handleEnterKey currentSymbol
  | null currentSymbol = newSymbolEntryOutput currentSymbol False Nothing
  | otherwise = newSymbolEntryOutput "" True (Just currentSymbol)

handleBackspaceKey :: Symbol -> SymbolEntryOutput
handleBackspaceKey currentSymbol = newSymbolEntryOutput nextSymbol nextIsDirty Nothing
  where
    (nextSymbol, nextIsDirty)
      | null currentSymbol = (currentSymbol, False)
      | otherwise = (init currentSymbol, True)

handleEscapeKey :: Symbol -> SymbolEntryOutput
handleEscapeKey currentSymbol = newSymbolEntryOutput "" True Nothing

handleCharKey :: Maybe Key -> Symbol -> SymbolEntryOutput
handleCharKey (Just (CharKey char)) symbol
  | isAlpha char = newSymbolEntryOutput (symbol ++ [char]) True Nothing
  | otherwise = newSymbolEntryOutput symbol False Nothing
handleCharKey _ symbol = newSymbolEntryOutput symbol False Nothing

newSymbolEntryOutput :: Symbol -> Dirty -> Maybe Symbol -> SymbolEntryOutput
newSymbolEntryOutput newSymbol newIsDirty maybeSymbol = SymbolEntryOutput
  { outputState = SymbolEntryState { pendingSymbol = newSymbol }
  , isDirty = newIsDirty
  , maybeEnteredSymbol = maybeSymbol
  }

