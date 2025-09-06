module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.Set (Set, empty, insert, member, delete)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Robot = Robot (IORef String)
data RunState = RunState (Set String)

initialState :: RunState
initialState = RunState empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
    name <- generateUniqueName
    nameRef <- liftIO $ newIORef name
    return (Robot nameRef)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
    RunState usedNames <- get
    oldName <- liftIO $ readIORef nameRef
    -- Remove the old name from the set
    let newUsedNames = delete oldName usedNames
    put (RunState newUsedNames)
    -- Generate a new unique name
    newName <- generateUniqueName
    liftIO $ writeIORef nameRef newName

robotName :: Robot -> IO String
robotName (Robot nameRef) = readIORef nameRef

-- Helper function to generate a unique name
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
    RunState usedNames <- get
    name <- liftIO generateRandomName
    if member name usedNames
        then generateUniqueName  -- Try again if name is already used
        else do
            put (RunState (insert name usedNames))
            return name

-- Helper function to generate a random name
generateRandomName :: IO String
generateRandomName = do
    letter1 <- randomRIO ('A', 'Z')
    letter2 <- randomRIO ('A', 'Z')
    digit1 <- randomRIO ('0', '9')
    digit2 <- randomRIO ('0', '9')
    digit3 <- randomRIO ('0', '9')
    return [letter1, letter2, digit1, digit2, digit3]
