module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad (replicateM)
import Control.Monad.State (StateT, get, put, modify')
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Random (randomRIO)

-- | A robot carries its (mutable) name in an IORef.
newtype Robot = Robot (IORef String)

-- | Run-time state: the collection of names currently in use.
type RunState = Set String

-- | No robots exist yet, so no names are taken.
initialState :: RunState
initialState = Set.empty

-- | Create a robot and give it a unique name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- generateUniqueName
  ref  <- liftIO $ newIORef name
  pure (Robot ref)

-- | Reset a robot's name, ensuring the new one is unique
--   and the old name is released.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  oldName <- liftIO $ readIORef ref
  modify' (Set.delete oldName)              -- free the old name
  newName <- generateUniqueName
  liftIO $ writeIORef ref newName

-- | Read the robot's current name.
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

-- | Produce a random robot name (two uppercase letters, three digits).
randomName :: IO String
randomName = do
  letters <- replicateM 2 randomUpper
  digits  <- replicateM 3 randomDigit
  pure (letters ++ digits)
  where
    randomUpper = randomRIO ('A', 'Z')
    randomDigit = randomRIO ('0', '9')

-- | Generate a name that is not already in use, recording it in the state.
generateUniqueName :: StateT RunState IO String
generateUniqueName = do
  taken <- get
  name  <- liftIO randomName
  if name `Set.member` taken
     then generateUniqueName
     else do
       put (Set.insert name taken)
       pure name
