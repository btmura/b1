module B1.Graphics.Rendering.OpenGL.LineSegment
  ( LineSegment(..)
  ) where

import B1.Graphics.Rendering.OpenGL.Point

data LineSegment =
  -- | Construct a line from one point to the other
  LineSegment Point Point
  deriving (Show)

