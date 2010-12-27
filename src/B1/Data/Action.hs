module B1.Data.Action
  ( Action(..)
  , comboAction
  ) where

-- | Action that when performed returns another action.
data Action = Action (IO Action)

-- | Combines two actions into a single action.
comboAction :: IO Action -> IO Action -> IO Action
comboAction action1 action2 = do
  Action nextAction1 <- action1
  Action nextAction2 <- action2
  return $ Action (comboAction nextAction1 nextAction2)


