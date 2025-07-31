module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (delete)

data Robot = Robot { nameRef :: IORef String }

data RunState = RunState { usedNames :: [String] }

initialState :: RunState
initialState = RunState { usedNames = [] }

mkRobot :: StateT RunState IO Robot
mkRobot = do
    state <- get
    newName <- liftIO $ generateUniqueName (usedNames state)
    let newState = state { usedNames = newName : usedNames state }
    put newState
    liftIO $ do
        ref <- newIORef newName
        return $ Robot { nameRef = ref }

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
    state <- get
    oldName <- liftIO $ readIORef ref
    newName <- liftIO $ generateUniqueName (delete oldName (usedNames state))
    let newState = state { usedNames = newName : delete oldName (usedNames state) }
    put newState
    liftIO $ writeIORef ref newName

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

-- Generate a random name in format AA000
generateName :: IO String
generateName = do
    letter1 <- randomRIO ('A', 'Z')
    letter2 <- randomRIO ('A', 'Z')
    digit1 <- randomRIO ('0', '9')
    digit2 <- randomRIO ('0', '9')
    digit3 <- randomRIO ('0', '9')
    return [letter1, letter2, digit1, digit2, digit3]

-- Generate a unique name not in the given list
generateUniqueName :: [String] -> IO String
generateUniqueName used = do
    name <- generateName
    if name `elem` used
        then generateUniqueName used
        else return name
