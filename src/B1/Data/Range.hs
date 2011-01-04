module B1.Data.Range
  ( gradualRange
  , linearRange
  ) where
 
-- | Get a gradual range containing values from start to end with step number
-- of elements. 
gradualRange :: Fractional a => a -> a -> Int -> [a]
gradualRange start end steps
  | steps < 0 = error "Number of steps cannot be negative."
  | otherwise = map (gradualValue start end (steps + 2)) [0 .. steps + 1]

gradualValue :: Fractional a => a -> a -> Int -> Int -> a
gradualValue start end numSteps step 
  | step == 0 = start
  | step == numSteps - 1 = end
  | otherwise = prevValue + (end - prevValue) / 2
  where
    prevValue = gradualValue start end numSteps (step - 1)

linearRange :: (Fractional a, Ord a) => a -> a -> Int -> [a]
linearRange start end steps
  | steps < 0 = error "Number of steps cannot be negative."
  | otherwise = map (linearValue start end (steps + 2)) [0 .. steps + 1]

linearValue :: (Fractional a, Ord a) => a -> a -> Int -> Int -> a
linearValue start end numSteps step
  | step == 0 = start
  | step == numSteps - 1 = end
  | start <= end = start + stepAmount * realToFrac step
  | otherwise = start - stepAmount * realToFrac step
  where
    stepAmount = abs (start - end) / realToFrac (numSteps - 1)
