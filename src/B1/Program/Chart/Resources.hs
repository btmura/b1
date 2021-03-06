module B1.Program.Chart.Resources
  ( Resources
    ( Resources
    , font
    , program
    , bufferManager
    , taskManager
    , windowWidth
    , windowHeight
    , mousePosition
    , previousMousePosition
    , mouseDragStartPosition
    )
  , newResources
  , updateKeysPressed
  , isKeyPressed
  , getKeyPressed
  , updateMouseButtonState
  , isMouseButtonPressed
  , isMouseButtonClicked
  , isMouseDrag
  , isMouseWheelMoving
  , hasMouseDragStarted
  , hasMouseDragFinished
  , updateMousePosition
  , invertMousePositionY
  , updateMouseWheelPosition
  , getMouseWheelVelocity
  , updateWindowSize
  ) where

import Control.Monad
import Data.Maybe
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Graphics.Rendering.FTGL
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

import B1.Control.TaskManager
import B1.Graphics.Rendering.OpenGL.BufferManager

dragThreshold = 10::Int 

data Resources = Resources
  { font :: Font
  , program :: Program
  , bufferManager :: BufferManager
  , taskManager :: TaskManager
  , windowWidth :: GLfloat
  , windowHeight :: GLfloat
  , keysPressed :: [Key]
  , previousKeysPressed :: [Key]
  , mouseButtonsPressed :: [MouseButton]
  , mouseButtonsReleased :: [MouseButton]
  , previousMouseButtonsPressed :: [MouseButton]
  , previousMouseButtonsReleased :: [MouseButton]
  , mousePosition :: (GLfloat, GLfloat)
  , previousMousePosition :: (GLfloat, GLfloat)
  , mouseDragStartPosition :: (GLfloat, GLfloat)
  , mouseDragCount :: Int
  , previousMouseDragCount :: Int
  , mouseWheelPosition :: Int
  , previousMouseWheelPosition :: Int
  } deriving (Eq, Show)

newResources :: Font -> Program -> BufferManager -> TaskManager
    -> Resources
newResources font program bufferManager taskManager = Resources
  { font = font
  , program = program
  , bufferManager = bufferManager
  , taskManager = taskManager
  , windowWidth = 0
  , windowHeight = 0
  , keysPressed = []
  , previousKeysPressed = []
  , mouseButtonsPressed = []
  , mouseButtonsReleased = []
  , previousMouseButtonsPressed = []
  , previousMouseButtonsReleased = []
  , mousePosition = (0, 0)
  , previousMousePosition = (0, 0)
  , mouseDragStartPosition = (0, 0)
  , mouseDragCount = 0
  , previousMouseDragCount = 0
  , mouseWheelPosition = 0
  , previousMouseWheelPosition = 0
  }

updateKeysPressed :: [Key] -> Resources -> Resources
updateKeysPressed keysPressed
    resources@Resources { keysPressed = previousKeysPressed } = resources
  { keysPressed = keysPressed
  , previousKeysPressed = previousKeysPressed
  }

-- TODO: Rename to isKeyReleased
isKeyPressed :: Resources -> Key -> Bool
isKeyPressed
    resources@Resources
      { keysPressed = keysPressed
      , previousKeysPressed = previousKeysPressed
      }
    key = any (== key) keysPressed
        && not (any (== key) previousKeysPressed)

getKeyPressed :: Resources -> [Key] -> Maybe Key
getKeyPressed resources keys =
  case pressedKeys of
    (first:_) -> Just first
    otherwise -> Nothing
  where
    presses = map (isKeyPressed resources) keys
    indexedPresses = zip keys presses
    pressedKeys = map fst $ filter snd indexedPresses 

updateMouseButtonState :: [MouseButton] -> [MouseButton]
    -> Resources -> Resources
updateMouseButtonState buttonsPressed buttonsReleased
    resources@Resources
      { mouseButtonsPressed = previousButtonsPressed
      , mouseButtonsReleased = previousButtonsReleased
      , mousePosition = mousePosition
      , mouseDragStartPosition = previousMouseDragStartPosition
      , mouseDragCount = previousMouseDragCount
      } =
  resources
    { mouseButtonsPressed = buttonsPressed
    , mouseButtonsReleased = buttonsReleased
    , previousMouseButtonsReleased = previousButtonsReleased
    , previousMouseButtonsPressed = previousButtonsPressed
    , mouseDragStartPosition = mouseDragStartPosition
    , mouseDragCount = mouseDragCount
    , previousMouseDragCount = previousMouseDragCount
    }
  where
    mouseDragCount =
        if any (== ButtonLeft) buttonsPressed
          then previousMouseDragCount + 1
          else 0

    mouseDragStartPosition =
        if previousMouseDragCount == 0 && mouseDragCount > 0
          then mousePosition
          else previousMouseDragStartPosition

isMouseButtonPressed :: Resources -> MouseButton -> Bool
isMouseButtonPressed
    Resources { mouseButtonsPressed = mouseButtonsPressed }
    button =
  any (== button) mouseButtonsPressed

isMouseButtonClicked :: Resources -> MouseButton -> Bool
isMouseButtonClicked
    resources@Resources
      { mouseButtonsReleased = buttonsReleased
      , previousMouseButtonsReleased = previousButtonsReleased
      }
    button =
  any (== button) buttonsReleased
      && not (any (== button) previousButtonsReleased)
      && not (hasMouseDragFinished resources)

isMouseDrag :: Resources -> Bool
isMouseDrag Resources { mouseDragCount = dragCount } =
  dragCount >= dragThreshold

hasMouseDragStarted :: Resources -> Bool
hasMouseDragStarted
    Resources
      { mouseDragCount = dragCount
      , previousMouseDragCount = previousDragCount
      } =
  previousDragCount < dragThreshold && dragCount >= dragThreshold

hasMouseDragFinished :: Resources -> Bool
hasMouseDragFinished
    Resources
      { mouseDragCount = dragCount
      , previousMouseDragCount = previousDragCount
      } =
  dragCount == 0 && previousDragCount >= dragThreshold

updateMousePosition :: Position -> Resources -> Resources
updateMousePosition (Position x y)
    resources@Resources { mousePosition = previousMousePosition } = resources
  { mousePosition = (fromIntegral x, fromIntegral y)
  , previousMousePosition = previousMousePosition
  }

-- TODO: Integrate into updateMousePosition
invertMousePositionY :: Resources -> Resources
invertMousePositionY
    resources@Resources
      { windowHeight = windowHeight
      , mousePosition = (x, y)
      } = 
  resources { mousePosition = (x, windowHeight - y) }

updateMouseWheelPosition :: Int -> Resources -> Resources
updateMouseWheelPosition position
    resources@Resources
      { mouseWheelPosition = previousPosition
      } =
  resources
    { mouseWheelPosition = position
    , previousMouseWheelPosition = previousPosition
    }

isMouseWheelMoving :: Resources -> Bool
isMouseWheelMoving
    Resources
      { mouseWheelPosition = position
      , previousMouseWheelPosition = previousPosition
      } =
  position /= previousPosition

getMouseWheelVelocity :: Resources -> Int
getMouseWheelVelocity
    Resources
      { mouseWheelPosition = position
      , previousMouseWheelPosition = previousPosition
      } =
  previousPosition - position

updateWindowSize :: Size -> Resources -> Resources
updateWindowSize (Size width height) resources = resources
  { windowWidth = fromIntegral width
  , windowHeight = fromIntegral height
  }


