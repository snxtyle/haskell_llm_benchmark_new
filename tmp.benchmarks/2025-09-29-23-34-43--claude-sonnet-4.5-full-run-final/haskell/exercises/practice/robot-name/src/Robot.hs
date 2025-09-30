module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set

data Robot = Robot (IORef String)

data RunState = RunState (Set String)

initialState :: RunState
initialState = RunState Set.empty

-- Generate a random robot name in format: two uppercase letters + three digits
generateName :: IO String
generateName = do
  letter1 <- randomRIO ('A', 'Z')
  letter2 <- randomRIO ('A', 'Z')
  digit1 <- randomRIO ('0', '9')
  digit2 <- randomRIO ('0', '9')
  digit3 <- randomRIO ('0', '9')
  return [letter1, letter2, digit1, digit2, digit3]

-- Generate a unique name that hasn't been used before
generateUniqueName :: Set String -> IO String
generateUniqueName usedNames = do
  name <- generateName
  if Set.member name usedNames
    then generateUniqueName usedNames
    else return name

mkRobot :: StateT RunState IO Robot
mkRobot = do
  RunState usedNames <- get
  name <- liftIO $ generateUniqueName usedNames
  put (RunState (Set.insert name usedNames))
  ref <- liftIO $ newIORef name
  return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  RunState usedNames <- get
  newName <- liftIO $ generateUniqueName usedNames
  put (RunState (Set.insert newName usedNames))
  liftIO $ writeIORef ref newName

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
