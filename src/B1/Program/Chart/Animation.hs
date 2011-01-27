module B1.Program.Chart.Animation
  ( Animation
  , animateOnce
  , current
  , next
  ) where
  
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


