module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, get, modify)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- A robot holds a mutable reference to its current name.
newtype Robot = Robot (IORef String)

-- RunState tracks all currently allocated names to ensure uniqueness.
type RunState = Set String

-- The initial state starts with no allocated names.
initialState :: RunState
initialState = Set.empty

-- Generate a random robot name: two uppercase letters followed by three digits.
randomName :: IO String
randomName = do
  letters <- replicateM 2 (randomRIO ('A', 'Z'))
  digits  <- replicateM 3 (randomRIO ('0', '9'))
  pure (letters ++ digits)

-- Generate and reserve a unique name (not currently in the state).
-- If mOld is provided, avoid reusing that same name immediately after a reset.
getFreshName :: Maybe String -> StateT RunState IO String
getFreshName mOld = do
  s <- get
  candidate <- liftIO randomName
  if Set.member candidate s || Just candidate == mOld
     then getFreshName mOld
     else do
       modify (Set.insert candidate)
       pure candidate

-- Create a robot with a unique random name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- getFreshName Nothing
  ref <- liftIO (newIORef name)
  pure (Robot ref)

-- Reset a robot's name to a new unique random name.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  old <- liftIO $ readIORef ref
  -- Free the old name
  modify (Set.delete old)
  -- Assign a new unique name, distinct from the old
  new <- getFreshName (Just old)
  liftIO $ writeIORef ref new

-- Read the robot's current name.
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
