module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Char (chr, ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef

-- Robot is a mutable reference to its name (which may be Nothing if not yet assigned)
newtype Robot = Robot (IORef (Maybe String))

-- RunState tracks all assigned names to ensure uniqueness
newtype RunState = RunState (Set String)

initialState :: RunState
initialState = RunState Set.empty

-- Generate a random robot name in the format "AA000"
randomName :: IO String
randomName = do
    l1 <- randomRIO ('A', 'Z')
    l2 <- randomRIO ('A', 'Z')
    d1 <- randomRIO ('0', '9')
    d2 <- randomRIO ('0', '9')
    d3 <- randomRIO ('0', '9')
    return [l1, l2, d1, d2, d3]

-- Generate a unique name not in the given set
uniqueName :: Set String -> IO String
uniqueName used = do
    name <- randomName
    if Set.member name used
        then uniqueName used
        else return name

mkRobot :: StateT RunState IO Robot
mkRobot = do
    ref <- liftIO $ newIORef Nothing
    return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = liftIO $ writeIORef ref Nothing

robotName :: Robot -> IO String
robotName (Robot ref) = do
    mName <- readIORef ref
    case mName of
        Just name -> return name
        Nothing -> error "robotName must be called from within StateT RunState IO context to ensure uniqueness."

-- Helper for tests: robotName in StateT context, ensures uniqueness and assigns if needed
robotNameST :: Robot -> StateT RunState IO String
robotNameST (Robot ref) = do
    mName <- liftIO $ readIORef ref
    case mName of
        Just name -> return name
        Nothing -> do
            RunState used <- get
            name <- liftIO $ uniqueName used
            liftIO $ writeIORef ref (Just name)
            put (RunState (Set.insert name used))
            return name
