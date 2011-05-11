module B1.Graphics.Rendering.OpenGL.Vbo
  ( Vbo(..)
  , createBufferObject
  , renderVbo
  , deleteVbo
  ) where

import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL

type NumElements = Int

data Vbo = VertexVbo BufferObject PrimitiveMode NumElements

createBufferObject :: Storable a => [a] -> IO BufferObject
createBufferObject elements = do
  [vbo] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just vbo
  array <- newListArray (0, length elements - 1) elements
  withStorableArray array (\ptr ->
      bufferData ArrayBuffer $= (ptrsize elements, ptr, StaticDraw))
  bindBuffer ArrayBuffer $= Nothing
  putStrLn $ "Created VBO: " ++ show vbo
  return vbo
  where
    ptrsize [] = toEnum 0
    ptrsize (x:xs) = toEnum $ length elements * (sizeOf x)

deleteVbo :: Vbo -> IO ()
deleteVbo (VertexVbo bufferObject _ _) = do
  putStrLn $ "Deleting VBO: " ++ show bufferObject
  deleteObjectNames [bufferObject]

renderVbo :: Vbo -> IO Vbo
renderVbo vbo@(VertexVbo bufferObject primitiveMode numElements) = do
  bindBuffer ArrayBuffer $= Just bufferObject
  arrayPointer VertexArray $= vertexArrayDescriptor
  arrayPointer ColorArray $= colorArrayDescriptor
  drawArrays primitiveMode 0 $ fromIntegral numElements
  bindBuffer ArrayBuffer $= Nothing
  return vbo
  where
    vertexArrayDescriptor = VertexArrayDescriptor 2 Float 20 $ offset 0
    colorArrayDescriptor = VertexArrayDescriptor 3 Float 20 $ offset 8

offset x = plusPtr nullPtr x

