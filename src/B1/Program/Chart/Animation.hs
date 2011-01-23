module B1.Program.Chart.Animation
  ( Animation
  , getCurrentFrame
  , getNextAnimation
  , animateOnce
  ) where
  
import B1.Program.Chart.Dirty

type Animation a = [a]

getCurrentFrame :: Animation a -> a
getCurrentFrame [] = error "Animation does not have any frames."
getCurrentFrame (first:_) = first

getNextAnimation :: Animation a -> Animation a
getNextAnimation [] = []
getNextAnimation (_:rest) = rest

animateOnce :: [a] -> Animation (a, Dirty) 
animateOnce [] = error "Cannot create an animation out of an empty list."
animateOnce values = zip (init values) (repeat True)
    ++ zip (repeat (last values)) (repeat False)


