module B1.Data.Action
  ( Action(..)
  , combine
  ) where

-- | Action that when performed returns another action.
data Action a = Action (a -> IO (Action a))

-- | Combines two actions into a single action.
combine :: (a -> IO (Action a)) -> (a -> IO (Action a))
    -> a -> IO (Action a)
combine action1 action2 input = do
  Action nextAction1 <- action1 input
  Action nextAction2 <- action2 input
  return $ Action $ combine nextAction1 nextAction2

