module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set

-- Robot is a mutable reference to its name
newtype Robot = Robot (IORef String)

-- RunState tracks all names that have been assigned to ensure uniqueness
newtype RunState = RunState (Set String)

-- Initial state with no names assigned yet
initialState :: RunState
initialState = RunState Set.empty

-- Generate a random name in format: two uppercase letters + three digits
generateRandomName :: IO String
generateRandomName = do
    letter1 <- randomRIO ('A', 'Z')
    letter2 <- randomRIO ('A', 'Z')
    digit1 <- randomRIO ('0', '9')
    digit2 <- randomRIO ('0', '9')
    digit3 <- randomRIO ('0', '9')
    return [letter1, letter2, digit1, digit2, digit3]

-- Generate a unique name that hasn't been used before
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
    RunState usedNames <- get
    name <- liftIO generateRandomName
    if Set.member name usedNames
        then generateUniqueName  -- Try again if name is already used
        else do
            put (RunState (Set.insert name usedNames))
            return name

-- Create a new robot with a unique random name
mkRobot :: StateT RunState IO Robot
mkRobot = do
    name <- generateUniqueName
    nameRef <- liftIO $ newIORef name
    return (Robot nameRef)

-- Reset a robot's name to a new unique random name
resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
    newName <- generateUniqueName
    liftIO $ writeIORef nameRef newName

-- Get the current name of a robot
robotName :: Robot -> IO String
robotName (Robot nameRef) = readIORef nameRef
