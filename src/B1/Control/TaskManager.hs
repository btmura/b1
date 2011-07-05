module B1.Control.TaskManager
  ( TaskManager
  , newTaskManager
  , addTask
  , launchTasks
  ) where

import Control.Concurrent
import Control.Monad
import Data.IORef

type Task = IO ()

data TaskManager = TaskManager (IORef [Task]) (MVar Bool) deriving (Eq)

-- TODO: Add a counter to be able to tell instances apart...
instance Show TaskManager where
  show _ = "TaskManager"

newTaskManager :: IO TaskManager
newTaskManager = do
  workQueue <- newIORef []
  launchLock <- newMVar True
  return $ TaskManager workQueue launchLock

addTask :: TaskManager -> Task -> IO ()
addTask (TaskManager workQueueRef _) task =
  modifyIORef workQueueRef (\workQueue -> workQueue ++ [task])

launchTasks :: TaskManager -> IO ()
launchTasks (TaskManager workQueueRef launchLock) = do
  maybeLock <- tryTakeMVar launchLock
  case maybeLock of
    Just _ -> do
      task <- atomicModifyIORef workQueueRef nextTask
      startTask task
      putMVar launchLock True
      return ()
    _ -> return ()

nextTask :: [Task] -> ([Task], [Task])
nextTask workQueue =
  case workQueue of
    (first:rest) -> (rest, [first])
    _ -> ([], [])

startTask :: [Task] -> IO ()
startTask [] = return ()
startTask tasks = do
  forkIO $ mapM_ id tasks
  return ()

