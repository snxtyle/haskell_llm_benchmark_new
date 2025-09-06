module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random

data Robot = Robot (IORef String)

data RunState = RunState (Set String)

initialState :: RunState
initialState = RunState Set.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
  RunState used <- get
  name <- generateUnique used
  modify $ \(RunState s) -> RunState (Set.insert name s)
  ref <- liftIO $ newIORef name
  return $ Robot ref

generateUnique :: Set String -> StateT RunState IO String
generateUnique used = loop
  where
    loop = do
      c1 <- liftIO $ randomRIO ('A', 'Z')
      c2 <- liftIO $ randomRIO ('A', 'Z')
      d1 <- liftIO $ randomRIO (0, 9)
      d2 <- liftIO $ randomRIO (0, 9)
      d3 <- liftIO $ randomRIO (0, 9)
      let name = [c1, c2] ++ show d1 ++ show d2 ++ show d3
      if Set.member name used then loop else return name

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = do
  oldName <- liftIO $ readIORef ref
  RunState used <- get
  let used' = Set.delete oldName used
  name <- generateUnique used'
  modify $ \(RunState s) -> RunState (Set.insert name (Set.delete oldName s))
  liftIO $ writeIORef ref name

robotName :: Robot -> IO String
robotName (Robot ref) = readIORef ref
