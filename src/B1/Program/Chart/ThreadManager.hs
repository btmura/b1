module B1.Program.Chart.TaskManager
  ( TaskManager
  , newTaskManager
  ) where

data TaskManager = TaskManager (IORef [IO ()])

newTaskManager :: IO TaskManager
newTaskManager = do
  workQueue <- newIORef []
  return $ TaskManager workQueue

