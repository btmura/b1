module B1.Graphics.Rendering.OpenGL.Vbo
  ( NumElements
  , Renderable(..)
  , Vbo(..)
  , createBufferObject
  , deleteBufferObject
  , offset
  ) where

import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL

type NumElements = Int

data Vbo = VertexVbo BufferObject PrimitiveMode NumElements

class Renderable a where
  render :: a -> IO a

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

deleteBufferObject :: BufferObject -> IO ()
deleteBufferObject vbo = do
  putStrLn $ "Deleting VBO: " ++ show vbo
  deleteObjectNames [vbo]

offset x = plusPtr nullPtr x

instance Renderable Vbo where
  render vbo@(VertexVbo bufferObject primitiveMode numElements) = do
    bindBuffer ArrayBuffer $= Just bufferObject
    arrayPointer VertexArray $= vertexArrayDescriptor
    drawArrays primitiveMode 0 $ fromIntegral numElements
    bindBuffer ArrayBuffer $= Nothing
    return vbo
    where
      vertexArrayDescriptor = VertexArrayDescriptor 2 Float 8 $ offset 0
      
