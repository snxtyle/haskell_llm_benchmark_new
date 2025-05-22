module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data Robot = Robot { nameRef :: IORef String }

data RunState = RunState { usedNames :: Set String }

initialState :: RunState
initialState = RunState { usedNames = Set.empty }

-- Generate a random name in format: 2 uppercase letters + 3 digits
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
    state <- get
    name <- liftIO generateRandomName
    if Set.member name (usedNames state)
        then generateUniqueName  -- Try again if name exists
        else do
            put state { usedNames = Set.insert name (usedNames state) }
            return name

mkRobot :: StateT RunState IO Robot
mkRobot = do
    name <- generateUniqueName
    ref <- liftIO $ newIORef name
    return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
    state <- get
    -- Get old name and remove it from used names
    oldName <- liftIO $ readIORef (nameRef robot)
    put state { usedNames = Set.delete oldName (usedNames state) }
    -- Generate new name
    newName <- generateUniqueName
    -- Update robot's name
    liftIO $ writeIORef (nameRef robot) newName

robotName :: Robot -> IO String
robotName robot = readIORef (nameRef robot)
