module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, gets, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- Use IORef to store the mutable name (Maybe String) for each robot.
newtype Robot = Robot (IORef (Maybe String))

-- RunState holds the set of all currently assigned names to ensure uniqueness.
-- Use IORef for mutable shared state accessible within IO.
newtype RunState = RunState (IORef (Set String))

-- Initial state is an empty set of names wrapped in an IORef.
-- Note: This function itself doesn't run in IO, but creates the structure
-- needed for the stateful computations which will run in IO.
initialState :: IO RunState
initialState = RunState <$> newIORef Set.empty

-- Creates a new Robot instance (an IORef initialized to Nothing).
-- This runs within the StateT transformer, allowing access to RunState if needed,
-- but mkRobot itself doesn't need to interact with the used names set.
-- It operates within IO because it creates an IORef.
mkRobot :: StateT RunState IO Robot
mkRobot = liftIO $ Robot <$> newIORef Nothing

-- Resets the robot's name by setting its IORef back to Nothing.
-- This also runs within StateT and IO.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot nameRef) = liftIO $ writeIORef nameRef Nothing

-- Gets the robot's name. If it doesn't have one, generates a new unique name.
-- This function returns IO String because name generation involves randomness (IO)
-- and potentially accessing/modifying the shared RunState (also IO via IORef).
-- It needs access to the RunState to ensure name uniqueness.
robotName :: Robot -> StateT RunState IO String
robotName robot@(Robot nameRef) = do
    mName <- liftIO $ readIORef nameRef
    case mName of
        Just name -> return name
        Nothing   -> do
            -- Get the IORef for the set of used names from the RunState
            (RunState usedNamesRef) <- gets id
            newName <- generateUniqueName usedNamesRef
            liftIO $ writeIORef nameRef (Just newName)
            return newName

-- Helper function to generate a unique name.
-- Takes the IORef of the used name set.
-- Runs in IO because of random generation and IORef access.
-- Needs to be lifted into StateT where it's called.
generateUniqueName :: IORef (Set String) -> StateT RunState IO String
generateUniqueName usedNamesRef = do
    newName <- liftIO generateRandomName
    -- Atomically check if the name exists and insert it if it doesn't.
    -- atomicModifyIORef' returns the result of the pure function applied.
    -- Here, we return whether the insertion happened (True) or not (False).
    inserted <- liftIO $ atomicModifyIORef' usedNamesRef $ \usedNames ->
        if Set.member newName usedNames
        then (usedNames, False) -- Name exists, don't change set, return False
        else (Set.insert newName usedNames, True) -- Name new, insert, return True

    if inserted
    then return newName -- Name was unique and inserted
    else generateUniqueName usedNamesRef -- Name collision, try again

-- Helper function to generate a random name string (AA### format).
-- Runs in IO due to randomRIO.
generateRandomName :: IO String
generateRandomName = do
    c1 <- randomRIO ('A', 'Z')
    c2 <- randomRIO ('A', 'Z')
    n1 <- randomRIO ('0', '9')
    n2 <- randomRIO ('0', '9')
    n3 <- randomRIO ('0', '9')
    return [c1, c2, n1, n2, n3]

