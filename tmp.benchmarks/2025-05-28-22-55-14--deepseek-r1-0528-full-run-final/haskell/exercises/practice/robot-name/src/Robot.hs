module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import qualified Data.Set as Set
import Data.IORef
import System.Random (randomRIO)
import Control.Monad.State (StateT, get, modify, liftIO)
import Control.Monad (replicateM)

newtype Robot = Robot (IORef String)
type RunState = Set.Set String

initialState :: RunState
initialState = Set.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
    name <- generateUniqueName
    modify (Set.insert name)
    ref <- liftIO $ newIORef name
    return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
    oldName <- liftIO $ readIORef ref
    modify (Set.delete oldName)
    newName <- generateUniqueName
    modify (Set.insert newName)
    liftIO $ writeIORef ref newName

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref

generateUniqueName :: StateT RunState IO String
generateUniqueName = do
    used <- get
    name <- liftIO generateName
    if Set.member name used
        then generateUniqueName
        else return name

generateName :: IO String
generateName = do
    letters <- replicateM 2 $ randomRIO ('A', 'Z')
    digits <- replicateM 3 $ randomRIO ('0', '9')
    return (letters ++ digits)
