module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

data Robot = Robot { name :: String }
data RunState = RunState { usedNames :: Set String }

initialState :: RunState
initialState = RunState { usedNames = Set.empty }

-- Generate a random name in the format of two uppercase letters followed by three digits
generateName :: StateT RunState IO String
generateName = do
    state <- get
    let used = usedNames state
    newName <- liftIO $ do
        letters <- sequence [randomRIO ('A', 'Z') | _ <- [1, 2]]
        digits <- sequence [randomRIO ('0', '9') | _ <- [1, 2, 3]]
        return $ letters ++ digits
    if Set.member newName used
        then generateName
        else do
            modify (\s -> s { usedNames = Set.insert newName used })
            return newName

mkRobot :: StateT RunState IO Robot
mkRobot = do
    newName <- generateName
    return $ Robot { name = newName }

resetName :: Robot -> StateT RunState IO ()
resetName robot = do
    state <- get
    let oldName = name robot
    put $ state { usedNames = Set.delete oldName (usedNames state) }
    newName <- generateName
    return ()

robotName :: Robot -> IO String
robotName robot = return $ name robot
