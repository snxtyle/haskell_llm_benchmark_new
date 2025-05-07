module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef', modifyIORef')
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (chr) -- For random character generation

-- A Robot has a mutable reference to its current name (Maybe String)
-- and a reference to the shared set of all active robot names (IORef (Set String)).
-- The internal constructor Robot_Internal is not exported.
data Robot = Robot_Internal (IORef (Maybe String)) (IORef (Set String))

-- RunState holds the IORef for the global set of used names.
-- It's a Maybe type because the IORef is created on first use (lazily).
type RunState = Maybe (IORef (Set String))

-- initialState is Nothing, indicating the IORef for used names hasn't been created yet.
-- This is a pure value.
initialState :: RunState
initialState = Nothing

-- Helper function to get the IORef for the shared set of used names from RunState.
-- If the IORef hasn't been created yet (RunState is Nothing), it creates it,
-- stores it in RunState, and then returns it.
getSharedNamesRef :: StateT RunState IO (IORef (Set String))
getSharedNamesRef = do
    currentRunStateValue <- get
    case currentRunStateValue of
        Just ref -> return ref -- IORef already exists, return it
        Nothing -> do
            -- IORef doesn't exist, create it
            newRef <- liftIO $ newIORef Set.empty
            -- Store the new IORef in RunState for future calls
            put (Just newRef)
            return newRef

-- Generates a random character within a given range (inclusive).
randomCharInRange :: (Char, Char) -> IO Char
randomCharInRange (low, high) = chr <$> randomRIO (fromEnum low, fromEnum high)

-- Generates a random robot name in the format LLNNN (e.g., RX837).
generateName :: IO String
generateName = do
    c1 <- randomCharInRange ('A', 'Z')
    c2 <- randomCharInRange ('A', 'Z')
    n1 <- randomCharInRange ('0', '9')
    n2 <- randomCharInRange ('0', '9')
    n3 <- randomCharInRange ('0', '9')
    return [c1, c2, n1, n2, n3]

-- Tries to generate a new unique name and register it in the shared set of used names.
-- This function is called by robotName. It handles collisions by retrying with a new name.
generateAndRegisterUniqueName :: IORef (Set String) -> IO String
generateAndRegisterUniqueName usedNamesRef = do
    candidateName <- generateName -- Generate a potential name
    
    -- Atomically attempt to insert the candidateName into the set of used names.
    -- atomicModifyIORef' takes the IORef and a pure function.
    -- The pure function receives the current Set (currentNames) and returns a pair:
    --   1. The new Set (updated or unchanged).
    --   2. A result value. Here, 'Maybe String' indicates success (Just name) or collision (Nothing).
    insertionOutcome <- atomicModifyIORef' usedNamesRef $ \currentNames ->
        if Set.member candidateName currentNames
            then (currentNames, Nothing) -- Collision: name already in set, set unchanged, signal failure.
            else (Set.insert candidateName currentNames, Just candidateName) -- Success: name not in set, add it, signal success.
    
    case insertionOutcome of
        Just uniqueName -> return uniqueName -- Successfully generated and registered a unique name.
        Nothing         -> generateAndRegisterUniqueName usedNamesRef -- Collision occurred, recurse to try again.

-- mkRobot creates a new robot instance.
-- It ensures the shared set of names is initialized (via getSharedNamesRef).
-- The new robot initially has no name (its IORef (Maybe String) is set to Nothing).
mkRobot :: StateT RunState IO Robot
mkRobot = do
    -- Ensure the global IORef for shared names is initialized and get a reference to it.
    sharedNamesGlobalRef <- getSharedNamesRef
    -- Create an IORef for this specific robot's name, initialized to Nothing.
    robotSpecificNameRef <- liftIO $ newIORef Nothing
    -- Return the new Robot, containing its own name ref and the shared names ref.
    return (Robot_Internal robotSpecificNameRef sharedNamesGlobalRef)

-- resetName wipes the robot's current name and removes it from the shared set of used names.
-- The robot returns to its "factory settings" state regarding its name.
-- The next call to robotName for this robot will generate a new unique name.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot_Internal robotNameRef sharedNamesRef) = do
    -- This function operates in StateT RunState IO, but it primarily uses the sharedNamesRef
    -- already captured by the Robot data type. Calling getSharedNamesRef here isn't strictly
    -- necessary if mkRobot is guaranteed to have initialized it, but it wouldn't harm.
    
    mOldName <- liftIO $ readIORef robotNameRef -- Read the robot's current name.
    case mOldName of
        Just oldName -> do
            -- If the robot had a name, remove it from the shared set of used names.
            liftIO $ modifyIORef' sharedNamesRef (Set.delete oldName)
            -- Wipe the robot's individual name by setting its IORef to Nothing.
            liftIO $ writeIORef robotNameRef Nothing
        Nothing -> return () -- Robot had no name, so nothing to reset or remove.

-- robotName returns the robot's name.
-- If the robot doesn't have a name yet (e.g., it's new or has been reset),
-- a new unique name is generated, stored in the robot and the shared set, and then returned.
robotName :: Robot -> IO String
robotName (Robot_Internal robotNameRef sharedNamesRef) = do
    mCurrentName <- readIORef robotNameRef -- Read the robot's current name.
    case mCurrentName of
        Just name -> return name -- Robot already has a name, return it.
        Nothing -> do
            -- Robot needs a name. Generate a new unique one and register it.
            newName <- generateAndRegisterUniqueName sharedNamesRef
            -- Store the new name in this robot's IORef.
            liftIO $ writeIORef robotNameRef (Just newName)
            return newName
