module B1.Program.Chart.Screen
  ( drawScreen
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL
import System.Directory
import System.FilePath
import System.IO

import B1.Data.Action
import B1.Data.Range
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.LineSegment
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Config
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

import qualified B1.Program.Chart.Chart as C
import qualified B1.Program.Chart.ChartFrame as F
import qualified B1.Program.Chart.Graph as G
import qualified B1.Program.Chart.Header as H
import qualified B1.Program.Chart.Overlay as O
import qualified B1.Program.Chart.SideBar as S
import qualified B1.Program.Chart.SymbolEntry as E

configFileName = ".b1config"

drawScreen :: Resources -> IO (Action Resources Dirty, Dirty)
drawScreen resources = do 
  configLock <- newEmptyMVar
  homeDirectory <- getHomeDirectory
  let configPath = homeDirectory ++ [pathSeparator] ++ configFileName
  config <- readConfig configPath
  let graphBounds = Just $ Box (-1, 1) (1, -0.1)
      volumeBounds = Just $ Box (-1, -0.1) (1, -0.4)
      stochasticBounds = Just $ Box (-1, -0.4) (1, -0.7)
      weeklyStochasticBounds = Just $ Box (-1, -0.7) (1, -1)

      options = F.FrameOptions
        { F.chartOptions = C.ChartOptions
          { C.headerOptions = H.HeaderOptions
            { H.fontSize = 18
            , H.padding = 10 
            , H.statusStyle = H.LongStatus
            , H.button = H.AddButton
            }
          , C.graphOptions = G.GraphOptions
            { G.boundSet = G.GraphBoundSet
              { G.graphBounds = graphBounds
              , G.volumeBounds = volumeBounds
              , G.stochasticsBounds = stochasticBounds
              , G.weeklyStochasticsBounds = weeklyStochasticBounds
              , G.monthLineBounds = Just $ Box (-1, 1) (1, -1)
              , G.dividerLines =
                [ LineSegment (-1, -0.1) (1, -0.1)
                , LineSegment (-1, -0.4) (1, -0.4)
                , LineSegment (-1, -0.7) (1, -0.7)
                ]
              }
            , G.fontSize = 18
            , G.maybeOverlayOptions = Just $ O.OverlayOptions
              { O.boundSet = O.OverlayBoundSet
                { O.graphBounds = graphBounds
                , O.volumeBounds = volumeBounds
                , O.stochasticBounds = stochasticBounds
                , O.weeklyStochasticBounds = weeklyStochasticBounds
                }
              }
            }
          , C.showRefreshButton = True
          }
        , F.inScaleAnimation = incomingScaleAnimation
        , F.inAlphaAnimation = incomingAlphaAnimation
        , F.outScaleAnimation = outgoingScaleAnimation
        , F.outAlphaAnimation = outgoingAlphaAnimation
        }
  inputFrameState <- F.newFrameState options $ selectedSymbol config
  drawScreenLoop
      configPath
      S.SideBarInput
        { S.bounds = zeroBox
        , S.newSymbols = symbols config
        , S.selectedSymbol = selectedSymbol config
        , S.draggedSymbol = Nothing
        , S.refreshRequested = False
        , S.inputState = S.newSideBarState
        }
      F.FrameInput
        { F.bounds = zeroBox
        , F.alpha = 1
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

drawScreenLoop :: String -> S.SideBarInput -> F.FrameInput -> E.SymbolEntryInput
    -> ScreenState -> Resources -> IO (Action Resources Dirty, Dirty)
drawScreenLoop
    configPath
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

  frameOutput <- preservingMatrix $ do
    translateToCenter frameBounds
    F.drawChartFrame resources frameInput { F.bounds = frameBounds }

  sideBarOutput <- preservingMatrix $ do
    translateToCenter sideBarBounds
    S.drawSideBar resources sideBarInput { S.bounds = sideBarBounds }

  symbolEntryOutput <- preservingMatrix $ do
    translateToCenter frameBounds
    E.drawSymbolEntry resources symbolEntryInput { E.bounds = frameBounds }

  let nextSymbols = S.symbols sideBarOutput
      nextSelectedSymbol =
          case F.selectedSymbol frameOutput of
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
      writeConfig configPath nextConfig
      takeMVar configLock
      return ()
    return ()

  let nextSideBarInput = sideBarInput
        { S.newSymbols = maybeToList $ F.buttonClickedSymbol frameOutput
        , S.selectedSymbol = F.selectedSymbol frameOutput
        , S.draggedSymbol = F.draggedSymbol frameOutput
        , S.refreshRequested = isJust $ F.refreshedSymbol frameOutput
        , S.inputState = S.outputState sideBarOutput
        }
      nextMaybeSymbolRequest = listToMaybe $ catMaybes
          [ S.symbolRequest sideBarOutput
          , F.refreshedSymbol frameOutput
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
  return (Action (drawScreenLoop configPath nextSideBarInput nextFrameInput
      nextSymbolEntryInput nextScreenState), nextDirty)

  where
    (sideBarWidth, sideBarWidthDirty) = current sideBarWidthAnimation

    nextSideBarOpen = not $ null slots
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


