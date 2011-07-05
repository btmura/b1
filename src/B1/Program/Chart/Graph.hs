module B1.Program.Chart.Graph
  ( GraphInput(..)
  , GraphOutput(..)
  , GraphOptions(..)
  , GraphState
  , GraphBoundSet(..)
  , drawGraph
  , newGraphState
  , cleanGraphState
  ) where

import Control.Monad
import Data.Maybe
import Graphics.Rendering.OpenGL

import B1.Data.List
import B1.Data.Price
import B1.Data.Range
import B1.Data.Technicals.MovingAverage
import B1.Data.Technicals.Stochastic
import B1.Data.Technicals.StockData
import B1.Graphics.Rendering.OpenGL.Box
import B1.Graphics.Rendering.OpenGL.BufferManager
import B1.Graphics.Rendering.OpenGL.LineSegment
import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Graphics.Rendering.FTGL.Utils
import B1.Program.Chart.Animation
import B1.Program.Chart.Colors
import B1.Program.Chart.Dirty
import B1.Program.Chart.FragmentShader
import B1.Program.Chart.Resources
import B1.Program.Chart.StochasticColors
import B1.Program.Chart.Vbo

import qualified B1.Program.Chart.Candlesticks as C
import qualified B1.Program.Chart.MovingAverageLines as M
import qualified B1.Program.Chart.MonthLines as L
import qualified B1.Program.Chart.Overlay as O
import qualified B1.Program.Chart.PriceLines as P
import qualified B1.Program.Chart.StochasticLines as S
import qualified B1.Program.Chart.VolumeBars as V

data GraphInput = GraphInput
  { bounds :: Box
  , alpha :: GLfloat
  , inputState :: GraphState
  }

data GraphOutput = GraphOutput
  { outputState :: GraphState
  , isDirty :: Dirty
  }

data GraphOptions = GraphOptions
  { boundSet :: GraphBoundSet
  , fontSize :: Int
  , maybeOverlayOptions :: Maybe O.OverlayOptions
  }

data GraphBoundSet = GraphBoundSet
  { graphBounds :: Maybe Box
  , volumeBounds :: Maybe Box
  , stochasticsBounds :: Maybe Box
  , weeklyStochasticsBounds :: Maybe Box
  , monthLineBounds :: Maybe Box
  , dividerLines :: [LineSegment]
  }

data GraphState = GraphState
  { options :: GraphOptions
  , stockData :: StockData
  , stockDataStatus :: StockDataStatus
  , bufferManager :: BufferManager
  , maybeVbo :: Maybe Vbo
  , maybeOverlayState :: Maybe O.OverlayState
  , loadingAlphaAnimation :: Animation (GLfloat, Dirty)
  , contentAlphaAnimation :: Animation (GLfloat, Dirty)
  }

newGraphState :: GraphOptions -> StockData -> BufferManager -> GraphState
newGraphState
    options@GraphOptions { maybeOverlayOptions = maybeOverlayOptions }
    stockData
    bufferManager =
  GraphState
    { options = options
    , stockData = stockData
    , stockDataStatus = Loading
    , bufferManager = bufferManager
    , maybeVbo = Nothing
    , maybeOverlayState = case maybeOverlayOptions of
                            Just overlayOptions ->
                              Just $ O.newOverlayState overlayOptions stockData
                            _ -> Nothing
    , loadingAlphaAnimation = incomingAlphaAnimation
    , contentAlphaAnimation = animateOnce $ linearRange 0 0 1
    }

cleanGraphState :: GraphState -> IO GraphState
cleanGraphState
    state@GraphState
      { maybeVbo = maybeVbo
      , bufferManager = bufferManager
      } =
  case maybeVbo of
    Just vbo -> do
      deleteVbo bufferManager vbo
      return state { maybeVbo = Nothing }
    _ -> return state

drawGraph :: Resources -> GraphInput -> IO GraphOutput
drawGraph resources
    input@GraphInput
      { inputState = GraphState
        { stockData = stockData
        , stockDataStatus = stockDataStatus
        }
      } = do
  renderLoading resources input
  nextStatus <- getStockDataStatus stockData
  let renderContent = case nextStatus of
                        Data -> renderData
                        ErrorMessage -> renderError
                        _ -> renderNothing
  graphOutput <- renderContent resources input
  maybeOverlayOutput <- renderOverlay resources input
  return $ nextGraphOutput stockDataStatus graphOutput maybeOverlayOutput

renderOverlay :: Resources -> GraphInput -> IO (Maybe O.OverlayOutput)
renderOverlay resources
    input@GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = GraphState
        { stockDataStatus = stockDataStatus
        , maybeOverlayState = maybeOverlayState
        }
      } =
  case maybeOverlayState of
    Just overlayState -> do
      let overlayInput = O.OverlayInput
            { O.bounds = bounds
            , O.alpha = if stockDataStatus == Loading || alpha /= 1.0
                          then 0
                          else 1
            , O.inputState = overlayState
            }
      overlayOutput <- O.drawOverlay resources overlayInput
      return $ Just overlayOutput
    _ -> return Nothing

renderData :: Resources -> GraphInput -> IO GraphOutput
renderData
    resources@Resources { program = program }
    input@GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@GraphState
        { options = GraphOptions { boundSet = boundSet }
        , stockData = stockData
        , bufferManager = bufferManager
        , maybeVbo = maybeVbo
        , loadingAlphaAnimation = loadingAlphaAnimation
        , contentAlphaAnimation = contentAlphaAnimation
        }
      }
    = do

  priceData <- liftM fromJust $ getStockPriceData stockData
  vbo <- maybe (createGraphVbo bufferManager boundSet priceData) return maybeVbo

  let finalAlpha = (min alpha . fst . current) contentAlphaAnimation
  preservingMatrix $ do
    scale3 (boxWidth bounds / 2) (boxHeight bounds / 2) 1 
    currentProgram $= Just program
    setAlpha program finalAlpha
    hasRendered <- renderVbo vbo
    currentProgram $= Nothing

    when hasRendered $ do
      let frameColor = outlineColor resources bounds finalAlpha
      mapM_ (\(LineSegment (x1, y1) (x2, y2)) -> do
          renderPrimitive Lines $ do
            color frameColor
            vertex $ vertex2 x1 y1
            vertex $ vertex2 x2 y2
          ) (dividerLines boundSet)

    return GraphOutput
      { outputState = state
        { stockDataStatus = if hasRendered then Data else Loading
        , maybeVbo = Just vbo
        }
      , isDirty = not hasRendered
      }

createGraphVbo :: BufferManager -> GraphBoundSet -> StockPriceData -> IO Vbo
createGraphVbo bufferManager boundSet priceData = 
  createVbo bufferManager $ concat
    [ getVboSpecList monthLineBounds $
        L.getVboSpecs priceData
    , getVboSpecList graphBounds $
        P.getVboSpecs priceData
    , getVboSpecList graphBounds $
        getGraphVboSpecs priceData
    , getVboSpecList volumeBounds $
        V.getVboSpecs priceData
    , getVboSpecList stochasticsBounds $
        S.getVboSpecs priceData dailySpecs 
    , getVboSpecList weeklyStochasticsBounds $
        S.getVboSpecs priceData weeklySpecs
    ]
  where
    getVboSpecList :: (GraphBoundSet -> Maybe Box) -> (Box -> [VboSpec])
        -> [VboSpec]
    getVboSpecList boundFunc vboFunc = case boundFunc boundSet of 
      Just bounds -> vboFunc bounds
      _ -> []

    getGraphVboSpecs :: StockPriceData -> Box -> [VboSpec]
    getGraphVboSpecs priceData bounds = C.getVboSpecs priceData bounds
        ++ M.getVboSpecs priceData bounds

    dailySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Daily
        , S.lineColor = red3
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Daily
        , S.lineColor = yellow3
        , S.stochasticFunction = d
        }
      ]

    weeklySpecs =
      [ S.StochasticLineSpec 
        { S.timeSpec = S.Weekly
        , S.lineColor = red3
        , S.stochasticFunction = k
        }
      , S.StochasticLineSpec
        { S.timeSpec = S.Weekly
        , S.lineColor = purple3
        , S.stochasticFunction = d
        }
      ]

renderError :: Resources -> GraphInput -> IO GraphOutput
renderError resources
    GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@GraphState
        { options = GraphOptions
          { fontSize = fontSize
          }
        , loadingAlphaAnimation = loadingAlphaAnimation
        , contentAlphaAnimation = contentAlphaAnimation
        }
      } =  do
  renderAlphaText resources bounds alpha contentAlphaAnimation red4 fontSize
      "ERROR"
  return GraphOutput
    { outputState = state { stockDataStatus = ErrorMessage }
    , isDirty = False
    }

renderNothing :: Resources -> GraphInput -> IO GraphOutput
renderNothing resources input@GraphInput { inputState = state } =
  return GraphOutput
    { outputState = state
    , isDirty = False
    }

renderLoading :: Resources -> GraphInput -> IO ()
renderLoading resources 
    GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = GraphState
        { options = GraphOptions
          { fontSize = fontSize
          }
        , loadingAlphaAnimation = loadingAlphaAnimation
        }
      } =
  renderAlphaText resources bounds alpha loadingAlphaAnimation gray4 fontSize
      "Loading DATA..."

renderAlphaText :: Resources -> Box -> GLfloat -> Animation (GLfloat, Dirty)
    -> (GLfloat -> Color4 GLfloat) -> Int -> String -> IO ()
renderAlphaText resources bounds alpha alphaAnimation
    textColor fontSize text = do
  -- TODO: Make a function in Animation to get the final alpha value.
  let finalAlpha = (min alpha . fst . current) alphaAnimation
  when (finalAlpha >= 0) $ do
    color $ textColor finalAlpha
    renderCenteredText resources bounds fontSize text

renderCenteredText :: Resources -> Box -> Int -> String -> IO ()
renderCenteredText resources bounds fontSize text = do
  let textSpec = TextSpec (font resources) fontSize text
  textBounds <- measureText textSpec
  when (boxWidth bounds > boxWidth textBounds) $ do
    preservingMatrix $ do
      let centerX = -(boxWidth textBounds / 2)
          centerY = -(boxHeight textBounds / 2)
      translate $ vector3 centerX centerY 0
      renderText textSpec

nextGraphOutput :: StockDataStatus -> GraphOutput -> Maybe O.OverlayOutput
    -> GraphOutput
nextGraphOutput prevStockDataStatus
    output@GraphOutput
      { outputState = state@GraphState
        { stockDataStatus = stockDataStatus
        , loadingAlphaAnimation = loadingAlphaAnimation
        , contentAlphaAnimation = contentAlphaAnimation
        }
      , isDirty = isDirty
      }
    maybeOverlayOutput =
  let nextLoadingAlphaAnimation = if prevStockDataStatus == Loading
                                      && stockDataStatus /= Loading
                                    then fadeOut loadingAlphaAnimation
                                    else next loadingAlphaAnimation
      nextContentAlphaAnimation = if prevStockDataStatus == Loading
                                      && stockDataStatus /= Loading
                                    then incomingAlphaAnimation
                                    else next contentAlphaAnimation
      animationsDirty = any (snd . current)
        [ nextLoadingAlphaAnimation
        , nextContentAlphaAnimation
        ]

      nextMaybeOverlayState = case maybeOverlayOutput of
                                Just overlayOutput ->
                                  Just $ O.outputState overlayOutput
                                _ -> Nothing
      overlayDirty = case maybeOverlayOutput of
                       Just overlayOutput -> O.isDirty overlayOutput
                       _ -> False
  in output
    { outputState = state
      { maybeOverlayState = nextMaybeOverlayState
      , loadingAlphaAnimation = nextLoadingAlphaAnimation
      , contentAlphaAnimation = nextContentAlphaAnimation
      }
    , isDirty = isDirty || animationsDirty || overlayDirty
    }

-- TODO: Make a function in Animation to make smooth transitions
fadeOut :: Animation (GLfloat, Dirty) -> Animation (GLfloat, Dirty) 
fadeOut alphaAnimation =
  let currentValue = fst $ current alphaAnimation
  in animateOnce $ linearRange currentValue 0 10

