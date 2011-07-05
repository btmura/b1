module B1.Graphics.Rendering.OpenGL.BufferManager
  ( BufferManager
  , newBufferManager
  , getOrCreateBindedBuffer
  , recycleBuffer
  ) where

import Data.IORef
import Data.List
import Foreign.Ptr
import Graphics.Rendering.OpenGL

data Buffer = Buffer BufferObject Int

instance Eq Buffer where
  (Buffer _ size) == (Buffer _ otherSize) = size == otherSize

instance Ord Buffer where
  compare (Buffer _ size) (Buffer _ otherSize) = compare size otherSize

data BufferManager = BufferManager (IORef [Buffer]) deriving (Eq)

-- TODO: Add a counter to be able to tell different instances apart...
instance Show BufferManager where
  show bufferManager = "BufferManager"

newBufferManager :: IO BufferManager
newBufferManager = do
  freeBuffersRef <- newIORef []
  return $ BufferManager freeBuffersRef

getOrCreateBindedBuffer :: BufferManager -> Int -> IO BufferObject
getOrCreateBindedBuffer (BufferManager freeBuffersRef) size = do
  maybeBufferObject <- atomicModifyIORef freeBuffersRef (nextBuffer size)
  case maybeBufferObject of
    Just bufferObject -> do
      putStrLn $ "Using recycled buffer: " ++ show bufferObject
      bindBuffer ArrayBuffer $= Just bufferObject
      return bufferObject
    _ -> do
      [bufferObject] <- genObjectNames 1
      putStrLn $ "Created new buffer: " ++ show bufferObject
          ++ " Size: " ++ show size
      bindBuffer ArrayBuffer $= Just bufferObject
      bufferData ArrayBuffer $= (fromIntegral size, nullPtr, DynamicDraw)
      return  bufferObject

nextBuffer :: Int -> [Buffer] -> ([Buffer], Maybe BufferObject)
nextBuffer size buffers =
  let (tooSmall, bigEnough) = partition (isTooSmall size) buffers
  in case bigEnough of
      ((Buffer bufferObject _):rest) -> (tooSmall ++ rest, Just bufferObject)
      _ -> (buffers, Nothing)

isTooSmall :: Int -> Buffer -> Bool
isTooSmall size (Buffer _ bufferSize) = bufferSize < size

recycleBuffer :: BufferManager -> BufferObject -> IO ()
recycleBuffer (BufferManager freeBuffersRef) bufferObject = do
  bindBuffer ArrayBuffer $= Just bufferObject
  (size, _, _) <- get $ bufferData ArrayBuffer
  bindBuffer ArrayBuffer $= Nothing
  putStrLn $ "Recycling buffer: " ++ show bufferObject
      ++ " Size: " ++ show size

  let newBuffer = Buffer bufferObject $ fromIntegral size
  modifyIORef freeBuffersRef (\freeBuffers -> insert newBuffer freeBuffers)

