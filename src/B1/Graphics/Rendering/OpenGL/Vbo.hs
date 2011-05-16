module B1.Graphics.Rendering.OpenGL.Vbo
  ( Vbo
  , createVbo
  , renderVbo
  , deleteVbo
  ) where

import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL

type NumElements = Int

data Vbo = Vbo BufferObject PrimitiveMode NumElements

createVbo :: Storable a => PrimitiveMode -> [a] -> IO Vbo
createVbo primitiveMode elements = do
  [bufferObject] <- genObjectNames 1
  bindBuffer ArrayBuffer $= Just bufferObject
  array <- newListArray (0, length elements - 1) elements
  withStorableArray array (\ptr ->
      bufferData ArrayBuffer $= (ptrsize elements, ptr, StaticDraw))
  bindBuffer ArrayBuffer $= Nothing
  putStrLn $ "Created VBO: " ++ show bufferObject
  return $ Vbo bufferObject primitiveMode numElements
  where
    ptrsize [] = toEnum 0
    ptrsize (x:xs) = toEnum $ length elements * (sizeOf x)
    numElements = length elements `div` 5

deleteVbo :: Vbo -> IO ()
deleteVbo (Vbo bufferObject _ _) = do
  putStrLn $ "Deleting VBO: " ++ show bufferObject
  deleteObjectNames [bufferObject]

renderVbo :: Vbo -> IO Vbo
renderVbo vbo@(Vbo bufferObject primitiveMode numElements) = do
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

