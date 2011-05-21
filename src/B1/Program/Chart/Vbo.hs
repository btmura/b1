module B1.Program.Chart.Vbo
  ( Vbo
  , createVbo
  , renderVbo
  , deleteVbo
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Array.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL

type NumElements = Int

data Vbo = Vbo BufferObject (MVar Bool) PrimitiveMode NumElements

createVbo :: PrimitiveMode -> Int -> [GLfloat] -> IO Vbo
createVbo primitiveMode size elements = do
  [bufferObject] <- genObjectNames 1
  putStrLn $ "Created VBO: " ++ show bufferObject

  bindBuffer ArrayBuffer $= Just bufferObject
  bufferData ArrayBuffer $= (numBytes, nullPtr, StaticDraw)
  maybePtr <- mapBuffer ArrayBuffer WriteOnly
  bindBuffer ArrayBuffer $= Nothing
  
  unmapMVar <- newEmptyMVar
  case maybePtr of
    Just ptr -> do
      forkIO $ do
        pokeArray ptr elements
        putMVar unmapMVar True
        putStrLn $ "Size: " ++ show size
            ++ " Real size: " ++ show (length elements)
      return ()
    _ ->
      putStrLn "Couldn't map buffer..."

  return $ Vbo bufferObject unmapMVar primitiveMode numElements
  where
    numBytes = toEnum $ size * 4
    numElements = size `div` 5

deleteVbo :: Vbo -> IO ()
deleteVbo (Vbo bufferObject unmapMVar _ _) = do
  putStrLn $ "Deleting VBO: " ++ show bufferObject
  unmap <- takeMVar unmapMVar
  unmapIfNecessary unmap bufferObject
  deleteObjectNames [bufferObject]

unmapIfNecessary :: Bool -> BufferObject -> IO ()
unmapIfNecessary unmap bufferObject = do
  bindBuffer ArrayBuffer $= Just bufferObject
  when unmap $ do
    unmapBuffer ArrayBuffer
    return ()
  bindBuffer ArrayBuffer $= Nothing

renderVbo :: Vbo -> IO Bool
renderVbo vbo@(Vbo bufferObject unmapMVar primitiveMode numElements) = do
  maybeUnmap <- tryTakeMVar unmapMVar
  case maybeUnmap of
    Just unmap -> do
      tryPutMVar unmapMVar False
      unmapIfNecessary unmap bufferObject

      bindBuffer ArrayBuffer $= Just bufferObject
      arrayPointer VertexArray $= vertexArrayDescriptor
      arrayPointer ColorArray $= colorArrayDescriptor
      drawArrays primitiveMode 0 $ fromIntegral numElements
      bindBuffer ArrayBuffer $= Nothing

      return True
    _ ->
      return False
  where
    vertexArrayDescriptor = VertexArrayDescriptor 2 Float 20 $ offset 0
    colorArrayDescriptor = VertexArrayDescriptor 3 Float 20 $ offset 8

offset x = plusPtr nullPtr x

