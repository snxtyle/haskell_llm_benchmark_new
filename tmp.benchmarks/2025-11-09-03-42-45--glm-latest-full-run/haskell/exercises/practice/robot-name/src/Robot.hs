module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import qualified Data.Set as Set
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | A mutable variable holding the robot's name.
data Robot = Robot (IORef String)

-- | The state for the robot factory, containing a set of all used names.
data RunState = RunState (Set.Set String)

-- | The initial state, with no names used.
initialState :: RunState
initialState = RunState Set.empty

-- | Generates a random name in the format "AA111".
generateRandomName :: IO String
generateRandomName = do
  letter1 <- randomRIO ('A', 'Z')
  letter2 <- randomRIO ('A', 'Z')
  digit1  <- randomRIO ('0', '9')
  digit2  <- randomRIO ('0', '9')
  digit3  <- randomRIO ('0', '9')
  return [letter1, letter2, digit1, digit2, digit3]

-- | Generates a unique name, adds it to the state, and returns it.
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
  name <- liftIO generateRandomName
  RunState usedNames <- get
  if name `Set.member` usedNames
    then generateUniqueName -- If the name is taken, try again.
    else do
      put (RunState (Set.insert name usedNames)) -- Add the new name to the state.
      return name

-- | Creates a new robot with a unique, random name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- generateUniqueName
  ref <- liftIO $ newIORef name
  return (Robot ref)

-- | Resets a robot's name to a new, unique random name.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  -- 1. Get the old name and remove it from the set of used names.
  oldName <- liftIO $ readIORef ref
  RunState usedNames <- get
  let updatedNames = Set.delete oldName usedNames
  put (RunState updatedNames)

  -- 2. Generate a new unique name and update the robot's state.
  newName <- generateUniqueName
  liftIO $ writeIORef ref newName

-- | Retrieves the current name of a robot.
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
