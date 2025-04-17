module Robot
  ( Robot
  , initialState
  , mkRobot
  , resetName
  , robotName
  ) where

import Control.Monad          (replicateM)
import Control.Monad.State    (StateT, get, liftIO, put)
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as S
import System.Random          (randomRIO)

-- | A robot stores its current name in an IORef so the name can be
--   mutated (reset) while the robot value itself stays the same.
newtype Robot = Robot (IORef String)

-- | Run‑time state shared by all robots created within the same
--   StateT run.  We merely keep the set of names that are already in
--   use so we can avoid collisions.
newtype RunState = RunState { usedNames :: S.Set String }

-- | When we start, no names have been assigned.
initialState :: RunState
initialState = RunState S.empty

-- | Produce a fresh robot with a unique name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  name <- getUniqueName
  ref  <- liftIO $ newIORef name
  pure (Robot ref)

-- | Reset a robot back to factory settings (new unique name).
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  -- Remove the old name from the set (if any).
  oldName <- liftIO $ readIORef ref
  RunState names <- get
  let names' = S.delete oldName names
  put (RunState names')
  -- Generate & store a new unique name.
  newName <- getUniqueName
  liftIO $ writeIORef ref newName

-- | Read the current name of a robot.
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

----------------------------------------------------------------------
-- helpers

-- | Generate a random name in the form “AA000”.
randomName :: IO String
randomName = do
  letters <- replicateM 2 $ randomRIO ('A', 'Z')
  digits  <- replicateM 3 $ randomRIO ('0', '9')
  pure (letters ++ digits)

-- | Get a fresh name that is guaranteed not to collide with the names
--   already stored in the RunState.
getUniqueName :: StateT RunState IO String
getUniqueName = do
  RunState names <- get
  tryUntilUnique names
  where
    tryUntilUnique :: S.Set String -> StateT RunState IO String
    tryUntilUnique names = do
      candidate <- liftIO randomName
      if candidate `S.member` names
        then tryUntilUnique names  -- collision: try again
        else do
          let names' = S.insert candidate names
          put (RunState names')
          pure candidate
