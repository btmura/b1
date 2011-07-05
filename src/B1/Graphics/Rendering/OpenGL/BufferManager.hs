module B1.Graphics.Rendering.OpenGL.BufferManager
  ( BufferManager
  , newBufferManager
  , addBuffer
  , getBuffer
  ) where

import Data.IORef
import Graphics.Rendering.OpenGL

data BufferManager = BufferManager (IORef [BufferObject]) deriving (Eq)

-- TODO: Add a counter to be able to tell different instances apart...
instance Show BufferManager where
  show bufferManager = "BufferManager"

newBufferManager :: IO BufferManager
newBufferManager = do
  freeBuffersRef <- newIORef []
  return $ BufferManager freeBuffersRef

addBuffer :: BufferManager -> BufferObject -> IO ()
addBuffer (BufferManager freeBuffersRef) newBuffer =
  modifyIORef freeBuffersRef (\freeBuffers -> newBuffer:freeBuffers)

getBuffer :: BufferManager -> IO (Maybe BufferObject)
getBuffer (BufferManager freeBuffersRef) =
  atomicModifyIORef freeBuffersRef
      (\freeBuffers -> case freeBuffers of
                         (first:rest) -> (rest, Just first)
                         _ -> ([], Nothing)
                       )

