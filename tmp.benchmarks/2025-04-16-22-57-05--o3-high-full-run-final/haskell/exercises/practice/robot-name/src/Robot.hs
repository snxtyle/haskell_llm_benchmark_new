module Robot
  ( Robot
  , RunState
  , initialState
  , mkRobot
  , resetName
  , robotName
  ) where

import Control.Monad          (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State    (StateT, get, modify')
import Data.IORef             (IORef, newIORef, readIORef, writeIORef)
import Data.Set               (Set)
import qualified Data.Set as Set
import System.Random          (randomRIO)

-- | A robot stores its (mutable) name in an IORef.
newtype Robot = Robot { nameRef :: IORef String }

-- | The running state keeps track of all names in use,
--   guaranteeing global uniqueness.
type RunState = Set String

-- | Start with no names in use.
initialState :: RunState
initialState = Set.empty

-- | Produce a random upper‑case letter.
randomLetter :: IO Char
randomLetter = randomRIO ('A', 'Z')

-- | Produce a random digit.
randomDigit :: IO Char
randomDigit  = randomRIO ('0', '9')

-- | Generate a random name of the form LLDDD.
generateName :: IO String
generateName = do
  letters <- replicateM 2 randomLetter
  digits  <- replicateM 3 randomDigit
  pure (letters ++ digits)

-- | Generate a name that is not already present in the supplied set.
uniqueName :: Set String -> IO String
uniqueName used = do
  name <- generateName
  if Set.member name used
     then uniqueName used
     else pure name

-- | Create a brand‑new robot with a unique name.
mkRobot :: StateT RunState IO Robot
mkRobot = do
  used <- get
  name <- liftIO $ uniqueName used
  modify' (Set.insert name)
  ref <- liftIO $ newIORef name
  pure (Robot ref)

-- | Reset a robot to factory settings, giving it a new unique name.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  oldName <- liftIO $ readIORef ref
  modify' (Set.delete oldName)          -- free the old name
  used <- get                           -- get updated set
  newName <- liftIO $ uniqueName used
  liftIO $ writeIORef ref newName
  modify' (Set.insert newName)

-- | Retrieve the current name of a robot.
robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
