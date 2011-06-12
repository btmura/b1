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
  }

data GraphState = GraphState
  { options :: GraphOptions
  , stockData :: StockData
  , maybePriceData :: Maybe (Either StockPriceData String)
  , maybeVbo :: Maybe Vbo
  , hasRendered :: Bool
  , loadingAlphaAnimation :: Animation (GLfloat, Dirty)
  , graphAlphaAnimation :: Animation (GLfloat, Dirty)
  }

data GraphBoundSet = GraphBoundSet
  { graphBounds :: Maybe Box
  , volumeBounds :: Maybe Box
  , stochasticsBounds :: Maybe Box
  , weeklyStochasticsBounds :: Maybe Box
  , dividerLines :: [LineSegment]
  }

newGraphState :: GraphOptions -> StockData -> GraphState
newGraphState options stockData =
  GraphState
    { options = options
    , stockData = stockData
    , maybePriceData = Nothing
    , maybeVbo = Nothing
    , hasRendered = False
    , loadingAlphaAnimation = incomingAlphaAnimation
    , graphAlphaAnimation = animateOnce $ linearRange 0 0 1
    }

cleanGraphState :: GraphState -> IO GraphState
cleanGraphState state@GraphState { maybeVbo = maybeVbo } =
  case maybeVbo of
    Just vbo -> do
      deleteVbo vbo
      return state { maybeVbo = Nothing }
    _ -> return state

drawGraph :: Resources -> GraphInput -> IO GraphOutput
drawGraph resources input = do
  (maybeVbo, hasRendered) <- renderGraph resources input
  maybePriceData <- getStockPriceData (stockData (inputState input))
  return $ getOutput input maybeVbo hasRendered maybePriceData

getOutput :: GraphInput -> Maybe Vbo -> Bool
    -> Maybe (Either StockPriceData String) -> GraphOutput
getOutput
    input@GraphInput
      { inputState = inputState@GraphState
        { stockData = stockData
        , maybePriceData = maybePriceData
        , maybeVbo = maybeVbo
        , hasRendered = hasRendered
        , loadingAlphaAnimation = loadingAlphaAnimation
        , graphAlphaAnimation = graphAlphaAnimation
        }
      }
    nextMaybeVbo
    nextHasRendered
    maybeNewPriceData = output
  where
    nextMaybePriceData =
        if isJust maybeNewPriceData
          then maybeNewPriceData
          else maybePriceData

    nextLoadingAlphaAnimation =
        if not nextHasRendered
          then next loadingAlphaAnimation
          else if not hasRendered && nextHasRendered
            then smoothOutgoingAlphaAnimation loadingAlphaAnimation
            else next loadingAlphaAnimation

    -- TODO: Make a function in Animation to make smooth transitions
    smoothOutgoingAlphaAnimation :: Animation (GLfloat, Dirty)
        -> Animation (GLfloat, Dirty) 
    smoothOutgoingAlphaAnimation alphaAnimation =
      let currentValue = fst $ current alphaAnimation
      in animateOnce $ linearRange currentValue 0 10

    nextGraphAlphaAnimation =
        if not nextHasRendered
          then next graphAlphaAnimation
          else if not hasRendered && nextHasRendered
            then incomingAlphaAnimation
            else next graphAlphaAnimation

    nextIsDirty = not hasRendered
        || (snd . current) loadingAlphaAnimation
        || (snd . current) graphAlphaAnimation

    output = GraphOutput
      { outputState = inputState
        { maybePriceData = nextMaybePriceData
        , maybeVbo = nextMaybeVbo
        , hasRendered = nextHasRendered
        , loadingAlphaAnimation = nextLoadingAlphaAnimation
        , graphAlphaAnimation = nextGraphAlphaAnimation
        }
      , isDirty = nextIsDirty
      }

renderGraph :: Resources -> GraphInput -> IO (Maybe Vbo, Bool)
renderGraph resources
    input@GraphInput
      { alpha = alpha
      , inputState = GraphState
        { maybePriceData = maybePriceData
        }
      } =
  case maybePriceData of
    Just priceData -> do
      (maybeVbo, hasRendered) <- either (renderPriceData resources input)
          (renderError resources input) priceData
      -- May still need to render loading if the deferred VBO was not rendered
      unless hasRendered $ renderLoading resources input
      return (maybeVbo, hasRendered)

    _ -> do
      renderLoading resources input
      return (Nothing, False)

renderPriceData :: Resources -> GraphInput -> StockPriceData
    -> IO (Maybe Vbo, Bool)
renderPriceData
    resources@Resources { program = program }
    input@GraphInput
      { bounds = bounds
      , alpha = alpha
      , inputState = state@GraphState
        { options = GraphOptions
          { boundSet = boundSet
          }
        , maybeVbo = maybeVbo
        , graphAlphaAnimation = graphAlphaAnimation
        }
      }
    priceData = do

  vbo <- maybe (createGraphVbo boundSet priceData) return maybeVbo

  let finalAlpha = (min alpha . fst . current) graphAlphaAnimation
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

    return (Just vbo, hasRendered)

createGraphVbo :: GraphBoundSet -> StockPriceData -> IO Vbo
createGraphVbo boundSet priceData = 
  createVbo $ concat
    [ getVboSpecList graphBounds $
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

renderError :: Resources -> GraphInput -> String -> IO (Maybe Vbo, Bool)
renderError resources
    GraphInput
      { alpha = alpha
      , inputState = GraphState
        { options = GraphOptions
          { fontSize = fontSize
          }
        , graphAlphaAnimation = graphAlphaAnimation
        }
      }
    _ =  do
  renderAlphaText resources alpha graphAlphaAnimation red4 fontSize "ERROR"
  return (Nothing, True)

renderLoading :: Resources -> GraphInput -> IO ()
renderLoading resources 
    GraphInput
      { alpha = alpha
      , inputState = GraphState
        { options = GraphOptions
          { fontSize = fontSize
          }
        , loadingAlphaAnimation = loadingAlphaAnimation
        }
      } =
  renderAlphaText resources alpha loadingAlphaAnimation gray4 fontSize
      "Loading DATA..."

renderAlphaText :: Resources -> GLfloat -> Animation (GLfloat, Dirty)
    -> (GLfloat -> Color4 GLfloat) -> Int -> String -> IO ()
renderAlphaText resources alpha alphaAnimation textColor fontSize text = do
  -- TODO: Make a function in Animation to get the final alpha value.
  let finalAlpha = (min alpha . fst . current) alphaAnimation
  when (finalAlpha >= 0) $ do
    color $ textColor finalAlpha
    renderCenteredText resources fontSize text

renderCenteredText :: Resources -> Int -> String -> IO ()
renderCenteredText resources fontSize text = do
  let textSpec = TextSpec (font resources) fontSize text
  textBounds <- measureText textSpec
  preservingMatrix $ do
    let centerX = -(boxWidth textBounds / 2)
        centerY = -(boxHeight textBounds / 2)
    translate $ vector3 centerX centerY 0
    renderText textSpec


