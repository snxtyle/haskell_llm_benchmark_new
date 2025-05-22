module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import System.Random (randomRIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set

data Robot = Robot (IORef (Maybe String))

data RunState = RunState (Set.Set String)

initialState :: RunState
initialState = RunState Set.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
    nameRef <- liftIO $ newIORef Nothing
    return $ Robot nameRef

resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = do
    maybeName <- liftIO $ readIORef nameRef
    case maybeName of
        Nothing -> return ()
        Just name -> do
            RunState usedNames <- get
            put $ RunState (Set.delete name usedNames)
            liftIO $ writeIORef nameRef Nothing

robotName :: Robot -> IO String
robotName (Robot nameRef) = do
    maybeName <- readIORef nameRef
    case maybeName of
        Just name -> return name
        Nothing -> error "Robot has no name. This should not happen in normal usage."

generateUniqueName :: Set.Set String -> IO String
generateUniqueName usedNames = do
    name <- generateRandomName
    if Set.member name usedNames
        then generateUniqueName usedNames
        else return name

generateRandomName :: IO String
generateRandomName = do
    letter1 <- randomRIO ('A', 'Z')
    letter2 <- randomRIO ('A', 'Z')
    digit1 <- randomRIO (0, 9)
    digit2 <- randomRIO (0, 9)
    digit3 <- randomRIO (0, 9)
    return [letter1, letter2, show digit1 !! 0, show digit2 !! 0, show digit3 !! 0]
