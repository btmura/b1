module B1.Program.Chart.Vbo
  ( Vbo
  , VboSpec(..)
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

data Vbo = Vbo
  { bufferObject :: BufferObject
  , unmapMVar :: MVar Bool
  , primitiveGroups :: [(PrimitiveMode, NumElements)]
  }

type ArraySize = Int

data VboSpec = VboSpec PrimitiveMode ArraySize [GLfloat]

createVbo :: [VboSpec] -> IO Vbo
createVbo vboSpecs = do
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
        let allElements = concat $
                map (\(VboSpec _ _ elements) -> elements) vboSpecs
        pokeArray ptr allElements
        putMVar unmapMVar True
        putStrLn $ "Size: " ++ show totalSize
            ++ " Real size: " ++ show (length allElements)
      return ()
    _ ->
      putStrLn "Couldn't map buffer..."

  return Vbo
    { bufferObject = bufferObject
    , unmapMVar = unmapMVar
    , primitiveGroups = primitiveGroups
    }
  where
    totalSize = sum $ map (\(VboSpec _ size _) -> size) vboSpecs
    numBytes = toEnum $ totalSize * 4
    primitiveGroups = map
        (\(VboSpec primitiveMode size _) -> (primitiveMode, size `div` 5))
        vboSpecs

deleteVbo :: Vbo -> IO ()
deleteVbo
    Vbo
      { bufferObject = bufferObject
      , unmapMVar = unmapMVar
      } = do
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
renderVbo
    Vbo
      { bufferObject = bufferObject
      , unmapMVar = unmapMVar
      , primitiveGroups = primitiveGroups
      } = do
  maybeUnmap <- tryTakeMVar unmapMVar
  case maybeUnmap of
    Just unmap -> do
      tryPutMVar unmapMVar False
      unmapIfNecessary unmap bufferObject

      bindBuffer ArrayBuffer $= Just bufferObject
      arrayPointer VertexArray $= vertexArrayDescriptor
      arrayPointer ColorArray $= colorArrayDescriptor
      renderPrimitiveGroups primitiveGroups
      bindBuffer ArrayBuffer $= Nothing

      return True
    _ ->
      return False
  where
    vertexArrayDescriptor = VertexArrayDescriptor 2 Float 20 $ offset 0
    colorArrayDescriptor = VertexArrayDescriptor 3 Float 20 $ offset 8

renderPrimitiveGroups :: [(PrimitiveMode, NumElements)] -> IO ()
renderPrimitiveGroups primitiveGroups =
  mapM_ (\(offset, (primitiveMode, numElements)) ->
      drawArrays primitiveMode (fromIntegral offset) (fromIntegral numElements)
      ) (zip offsets primitiveGroups)
  where
    offsets = scanl (+) 0 $ map snd primitiveGroups

offset x = plusPtr nullPtr x

