module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL
import System.IO

import B1.Data.Action
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Config
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.ChartFrame as F
import qualified B1.Program.Chart.Header as H
import qualified B1.Program.Chart.SideBar as S
import qualified B1.Program.Chart.SymbolEntry as E

configFileName = ".b1config"

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = do 
  configLock <- newEmptyMVar
  config <- readConfig configFileName
  let chartOptions = C.ChartOptions
        { C.headerFontSize = 18
        , C.headerPadding = 10 
        , C.headerStatusStyle = H.LongStatus
        , C.headerButton = H.AddButton
        }
  inputFrameState <- F.newFrameState chartOptions $ selectedSymbol config
  drawScreenLoop
      S.SideBarInput
        { S.bounds = zeroBox
        , S.newSymbols = symbols config
--        , S.newMiniChartDraggedIn = Nothing
        , S.justSelectedSymbol = selectedSymbol config
        , S.refreshRequested = False
        , S.inputState = S.newSideBarState
        }
      F.FrameInput
        { F.bounds = zeroBox
        , F.maybeSymbolRequest = Nothing
        , F.inputState = inputFrameState
        } 
      E.SymbolEntryInput
        { E.bounds = zeroBox
        , E.inputState = E.newSymbolEntryState
        }
      ScreenState
        { sideBarOpen = False
        , sideBarWidthAnimation = animateOnce $ linearRange 0 0 30
        , config = config
        , configLock = configLock
        }
      resources

data ScreenState = ScreenState
  { sideBarOpen :: Bool
  , sideBarWidthAnimation :: Animation (GLfloat, Dirty)
  , config :: Config
  , configLock :: MVar Config
  }

sideBarOpenWidth = 150
openSideBarAnimation = animateOnce $ linearRange 0 sideBarOpenWidth 10
closeSideBarAnimation = animateOnce $ linearRange sideBarOpenWidth 0 10

drawScreenLoop :: S.SideBarInput -> F.FrameInput -> E.SymbolEntryInput
    -> ScreenState -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop
    sideBarInput@S.SideBarInput
      { S.inputState = S.SideBarState { S.slots = slots }
      }
    frameInput
    symbolEntryInput
    screenState@ScreenState
      { sideBarOpen = sideBarOpen
      , sideBarWidthAnimation = sideBarWidthAnimation
      , config = config
      , configLock = configLock
      }
    resources = do

  sideBarOutput <- preservingMatrix $ do
    translateToCenter sideBarBounds
    S.drawSideBar resources sideBarInput { S.bounds = sideBarBounds }

  frameOutput <- preservingMatrix $ do
    translateToCenter frameBounds
    F.drawChartFrame resources frameInput { F.bounds = frameBounds }

  symbolEntryOutput <- preservingMatrix $ do
    translateToCenter frameBounds
    E.drawSymbolEntry resources symbolEntryInput { E.bounds = frameBounds }

  let nextSymbols = S.symbols sideBarOutput
      nextSelectedSymbol =
          case F.maybeSelectedSymbol frameOutput of
            Just symbol -> Just symbol
            _ -> selectedSymbol config
      nextConfig = config
        { symbols = nextSymbols
        , selectedSymbol = nextSelectedSymbol
        }
  unless (config == nextConfig) $ do
    -- TODO: Extract this code into a separate ConfigManager module
    putStrLn $ "Saving configuration..."
    forkIO $ do
      putMVar configLock nextConfig
      writeConfig configFileName nextConfig
      takeMVar configLock
      return ()
    return ()

  let nextSideBarInput = sideBarInput
        { S.newSymbols = maybeToList $ F.maybeAddedSymbol frameOutput
--        , S.newMiniChartDraggedIn = Nothing
        , S.justSelectedSymbol = F.maybeSelectedSymbol frameOutput
        , S.refreshRequested = isJust $ F.maybeRefreshedSymbol frameOutput
        , S.inputState = S.outputState sideBarOutput
        }
      nextMaybeSymbolRequest = listToMaybe $ catMaybes
          [ S.symbolRequest sideBarOutput
          , F.maybeRefreshedSymbol frameOutput
          , E.maybeEnteredSymbol symbolEntryOutput
          ]
      nextFrameInput = frameInput
        { F.maybeSymbolRequest = nextMaybeSymbolRequest
        , F.inputState = F.outputState frameOutput
        }
      nextSymbolEntryInput = symbolEntryInput
        { E.inputState = E.outputState symbolEntryOutput
        }
      nextScreenState = screenState
        { sideBarOpen = nextSideBarOpen
        , sideBarWidthAnimation = nextSideBarWidthAnimation
        , config = nextConfig
        }
      nextDirty = sideBarWidthDirty
          || nextSideBarWidthDirty
          || S.isDirty sideBarOutput
          || E.isDirty symbolEntryOutput
          || F.isDirty frameOutput
  return (Action (drawScreenLoop nextSideBarInput nextFrameInput
      nextSymbolEntryInput nextScreenState), nextDirty)

  where
    (sideBarWidth, sideBarWidthDirty) = current sideBarWidthAnimation

    nextSideBarOpen = length slots > 0
    nextSideBarWidthAnimation
      | not sideBarOpen && nextSideBarOpen = openSideBarAnimation
      | sideBarOpen && not nextSideBarOpen = closeSideBarAnimation
      | otherwise = next sideBarWidthAnimation

    nextSideBarWidthDirty = snd . current $ nextSideBarWidthAnimation

    height = windowHeight resources

    sideBarTopPadding = 5

    sideBarBounds = Box (0, height - sideBarTopPadding) (sideBarWidth, 0)
    frameBounds = Box (sideBarWidth, height) (windowWidth resources, 0)

translateToCenter :: Box -> IO ()
translateToCenter bounds =
  translate $ vector3 translateX translateY 0
  where
    translateX = boxLeft bounds + boxWidth bounds / 2
    translateY = boxTop bounds - boxHeight bounds / 2


