module B1.Program.Chart.GraphUtilsTest
  ( getTestGroup
  ) where

import Control.Monad
import Graphics.Rendering.OpenGL
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import B1.Graphics.Rendering.OpenGL.Box
import B1.Program.Chart.GraphUtils

import qualified Test.Framework.Providers.API

getTestGroup :: Test.Framework.Providers.API.Test
getTestGroup = testGroup "B1.Program.Chart.GraphUtilsTest"
  [ testProperty "prop_lineStripPoint_bounds" prop_lineStripPoint_bounds
  , testProperty "prop_lineStripPoint_length" prop_lineStripPoint_length
  ]

prop_lineStripPoint_bounds :: Box -> [GLfloat] -> Int -> Int -> Property
prop_lineStripPoint_bounds bounds percentages size index = 
  size > 1 && size >= length percentages
      && index >= 0 && index < length percentages ==>
    let [x, y] = lineStripPoint bounds percentages size index
    in boxContains bounds (x, y)

prop_lineStripPoint_length :: Box -> [GLfloat] -> Int -> Int -> Bool
prop_lineStripPoint_length bounds percentages size index =
  let point = lineStripPoint bounds percentages size index
  in if null percentages
         || size < 2
         || index >= size
         || index >= length percentages
       then null point
       else length point == 2

instance Arbitrary Box where
  arbitrary = do
    left <- genGLfloat (0, 100)
    bottom <- genGLfloat (0, 100)
    width <- genGLfloat (0, 100)
    height <- genGLfloat (0, 100)
    return $ Box (left, bottom + height) (left + width, bottom)

genGLfloat :: (Float, Float) -> Gen GLfloat
genGLfloat range = liftM realToFrac $ choose range

