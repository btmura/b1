module B1.Control.TaskManager
  ( TaskManager
  , newTaskManager
  , addTask
  , launchTasks
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef

data TaskManager = TaskManager (IORef [IO ()])

newTaskManager :: IO TaskManager
newTaskManager = do
  workQueue <- newIORef []
  return $ TaskManager workQueue

addTask :: TaskManager -> IO () -> IO ()
addTask (TaskManager workQueueRef) task =
  modifyIORef workQueueRef (\workQueue -> workQueue ++ [task])

launchTasks :: TaskManager -> IO ()
launchTasks (TaskManager workQueueRef) = do
  tasks <- atomicModifyIORef workQueueRef (\workQueue -> ([], workQueue))
  unless (null tasks) $ do
    forkIO $ mapM_ id tasks
    return ()

