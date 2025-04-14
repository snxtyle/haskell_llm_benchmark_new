module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put)
import Control.Monad.IO.Class (liftIO)
import System.Random (randomRIO)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Robot = Robot Int
  deriving (Eq, Ord, Show)

data RunState = RunState
  { nextId :: Int
  , namesMap :: Map.Map Int (Maybe String)
  , assignedNames :: Set String
  }

initialState :: RunState
initialState = RunState 0 Map.empty Set.empty

-- Generate a random name in the format: two uppercase letters + three digits
randomName :: IO String
randomName = do
  letters <- sequence [randomRIO ('A','Z'), randomRIO ('A','Z')]
  digits <- sequence [randomRIO ('0','9'), randomRIO ('0','9'), randomRIO ('0','9')]
  return (letters ++ digits)

-- Generate a unique random name not in the assigned set
uniqueRandomName :: Set String -> IO String
uniqueRandomName assigned = do
  name <- randomName
  if name `Set.member` assigned
    then uniqueRandomName assigned
    else return name

mkRobot :: StateT RunState IO Robot
mkRobot = do
  st@RunState{nextId = nid, namesMap = nm} <- get
  let robot = Robot nid
  put st{ nextId = nid + 1, namesMap = Map.insert nid Nothing nm }
  return robot

resetName :: Robot -> StateT RunState IO ()
resetName (Robot rid) = do
  st@RunState{namesMap = nm, assignedNames = assigned} <- get
  case Map.lookup rid nm of
    Just (Just oldName) -> do
      let assigned' = Set.delete oldName assigned
          nm' = Map.insert rid Nothing nm
      put st{ namesMap = nm', assignedNames = assigned' }
    _ -> return ()

robotName :: Robot -> StateT RunState IO String
robotName (Robot rid) = do
  st@RunState{namesMap = nm, assignedNames = assigned} <- get
  case Map.lookup rid nm of
    Just (Just name) -> return name
    _ -> do
      name <- liftIO $ uniqueRandomName assigned
      let nm' = Map.insert rid (Just name) nm
          assigned' = Set.insert name assigned
      put st{ namesMap = nm', assignedNames = assigned' }
      return name
