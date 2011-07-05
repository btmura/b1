module B1.Program.Chart.Animation
  ( Animation
  , animateOnce
  , current
  , next
  , incomingScaleAnimation
  , incomingAlphaAnimation
  , outgoingScaleAnimation
  , outgoingAlphaAnimation
  ) where

import Graphics.Rendering.OpenGL
  
import B1.Data.Range
import B1.Program.Chart.Dirty

type Animation a = [a]

current :: Animation a -> a
current [] = error "Animation does not have any frames."
current (first:_) = first

next :: Animation a -> Animation a
next [] = []
next (_:rest) = rest

animateOnce :: [a] -> Animation (a, Dirty) 
animateOnce [] = error "Cannot create an animation out of an empty list."
animateOnce values = zip (init values) (repeat True)
    ++ zip (repeat (last values)) (repeat False)

incomingScaleAnimation :: Animation (GLfloat, Dirty)
incomingScaleAnimation = animateOnce $ linearRange 0.95 1 10

incomingAlphaAnimation :: Animation (GLfloat, Dirty)
incomingAlphaAnimation = animateOnce $ linearRange 0 1 10

outgoingScaleAnimation :: Animation (GLfloat, Dirty)
outgoingScaleAnimation = animateOnce $ linearRange 1 1.1 10

outgoingAlphaAnimation :: Animation (GLfloat, Dirty)
outgoingAlphaAnimation = animateOnce $ linearRange 1 0 10

