module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, get, put, modify')
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import System.Random (StdGen, mkStdGen, randomR)

-- A Robot is a mutable reference to its name.
-- The name is a Maybe String, because a new or reset robot has no name.
newtype Robot = Robot (IORef (Maybe String))

-- The RunState holds the global state for the robot factory:
-- 1. A random number generator for creating new names.
-- 2. A set of all names currently in use to ensure uniqueness.
data RunState = RunState StdGen (Set.Set String)

-- The initial state for the robot factory.
-- It starts with a deterministic random generator (for reproducibility)
-- and an empty set of used names.
initialState :: RunState
initialState = RunState (mkStdGen 0) Set.empty

-- Creates a new robot. The robot starts without a name.
-- The name will be generated on the first call to `robotName`.
mkRobot :: StateT RunState IO Robot
mkRobot = Robot <$> liftIO (newIORef Nothing)

-- Resets a robot to its factory settings.
-- This involves clearing its name and making the old name available again.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  mOldName <- liftIO $ readIORef ref
  -- If the robot had a name, remove it from the set of used names.
  case mOldName of
    Just oldName -> modify' (\(RunState gen used) -> RunState gen (Set.delete oldName used))
    Nothing -> return ()
  -- Clear the robot's name.
  liftIO $ writeIORef ref Nothing

-- Gets the name of a robot.
-- If the robot doesn't have a name, it generates a new unique one,
-- assigns it to the robot, and returns it.
-- NOTE: The signature has been changed from `IO String` to `StateT RunState IO String`
-- to allow access to the shared state for unique name generation.
robotName :: Robot -> StateT RunState IO String
robotName (Robot ref) = do
  mName <- liftIO $ readIORef ref
  case mName of
    Just name -> return name
    Nothing -> do
      newName <- newUniqueName
      liftIO $ writeIORef ref (Just newName)
      return newName

-- Helper function to generate a new name that is not already in use.
newUniqueName :: StateT RunState IO String
newUniqueName = do
  (RunState gen used) <- get
  let (name, nextGen) = generateName gen
  if Set.member name used
    then do
      -- If the name is already used, update the generator and try again.
      put (RunState nextGen used)
      newUniqueName
    else do
      -- If the name is unique, add it to the set and update the generator.
      put (RunState nextGen (Set.insert name used))
      return name

-- Helper function to generate a single random name in the format AA123.
generateName :: StdGen -> (String, StdGen)
generateName gen0 =
  let (c1, gen1) = randomR ('A', 'Z') gen0
      (c2, gen2) = randomR ('A', 'Z') gen1
      (n, gen3) = randomR (0, 999) gen2
      numStr = show n
      paddedNumStr = replicate (3 - length numStr) '0' ++ numStr
   in ([c1, c2] ++ paddedNumStr, gen3)
