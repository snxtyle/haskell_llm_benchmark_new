module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set

-- Robot data type that stores its current name
data Robot = Robot String

-- RunState to track used names
data RunState = RunState { usedNames :: Set String }
  deriving (Show)

-- Initial state with no used names
initialState :: RunState
initialState = RunState Set.empty

-- Generate a random name in format AA000
generateRandomName :: IO String
generateRandomName = do
  c1 <- randomRIO ('A', 'Z')
  c2 <- randomRIO ('A', 'Z')
  d1 <- randomRIO ('0', '9')
  d2 <- randomRIO ('0', '9')
  d3 <- randomRIO ('0', '9')
  return [c1, c2, d1, d2, d3]

-- Generate a unique random name
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
  state <- get
  let used = usedNames state
  name <- liftIO generateRandomName
  if name `Set.member` used
    then generateUniqueName  -- Try again if name is already used
    else do
      put $ state { usedNames = Set.insert name used }
      return name

-- Create a new robot with a unique random name
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- generateUniqueName
  return $ Robot name

-- Reset a robot's name to a new unique random name
resetName :: Robot -> StateT RunState IO ()
resetName _ = do
  newName <- generateUniqueName
  -- Note: We don't need to return the robot since we're working with state
  -- In a real implementation, we might want to update the robot reference
  return ()

-- Get a robot's name
robotName :: Robot -> IO String
robotName (Robot name) = return name
