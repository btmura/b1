module B1.Graphics.Rendering.OpenGL.BufferManager
  ( BufferManager
  , newBufferManager
  , addBuffer
  , getBuffer
  ) where

import Data.IORef
import Graphics.Rendering.OpenGL

data BufferManager = BufferManager (IORef [BufferObject])

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

