module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, put)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (delete)
import System.Random (randomRIO)

newtype Robot = Robot (IORef String)
type RunState = [String]

initialState :: RunState
initialState = []

-- Generate a random robot name: two uppercase letters followed by three digits.
genName :: IO String
genName = do
  l1 <- randomRIO ('A', 'Z')
  l2 <- randomRIO ('A', 'Z')
  ds <- replicateM 3 (randomRIO ('0', '9'))
  pure (l1 : l2 : ds)

-- Acquire a fresh name not present in the current RunState; update the state.
freshName :: StateT RunState IO String
freshName = do
  used <- get
  candidate <- liftIO genName
  if candidate `elem` used
    then freshName
    else do
      put (candidate : used)
      pure candidate

mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- freshName
  ref <- liftIO (newIORef name)
  pure (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  old <- liftIO (readIORef ref)
  used <- get
  -- Free the old name so it can be reused by other robots later.
  put (delete old used)
  -- Assign a fresh unique name and update both the robot and the state.
  new <- freshName
  liftIO (writeIORef ref new)

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
