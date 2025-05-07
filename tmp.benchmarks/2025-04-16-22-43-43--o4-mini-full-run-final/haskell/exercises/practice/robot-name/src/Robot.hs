module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, modify)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

-- RunState holds all names that have been assigned so far
type RunState = Set String

-- A Robot wraps an IORef to its current name
newtype Robot = Robot (IORef String)

-- At start, no names have been used
initialState :: RunState
initialState = Set.empty

-- Create a new robot: generate a fresh unique name and store it
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- genName
  ref  <- liftIO (newIORef name)
  return (Robot ref)

-- Reset a robot: generate a fresh unique name and overwrite
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  name <- genName
  liftIO (writeIORef ref name)

-- Read the robot’s current name
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

-- Helper to generate a unique name, updating the RunState
genName :: StateT RunState IO String
genName = do
  candidate <- liftIO generateName
  used      <- get
  if Set.member candidate used
    then genName
    else do
      modify (Set.insert candidate)
      return candidate

-- Generate a random name: two uppercase letters followed by three digits
generateName :: IO String
generateName = do
  l1 <- randomRIO ('A','Z')
  l2 <- randomRIO ('A','Z')
  d1 <- randomRIO ('0','9')
  d2 <- randomRIO ('0','9')
  d3 <- randomRIO ('0','9')
  return [l1, l2, d1, d2, d3]
