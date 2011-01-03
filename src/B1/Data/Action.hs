module B1.Data.Action
  ( Action(..)
  ) where

-- | Action that when performed returns another action.
-- a is a type parameter for some input that is required t yield the
-- next action, and b is an additional output that the caller using
-- the actions can use.
data Action a b = Action (a -> IO (Action a b, b)) 

