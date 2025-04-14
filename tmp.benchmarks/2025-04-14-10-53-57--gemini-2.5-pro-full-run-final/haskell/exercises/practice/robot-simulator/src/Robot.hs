module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum, Bounded)

-- Define the Robot data type to hold its state
data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }
             deriving (Eq, Show)

-- Function to create a new robot
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- Helper function to turn the robot right
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- Helper function to turn the robot left
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- Helper function to advance the robot one step in its current direction
advance :: Robot -> Robot
advance robot@(Robot b (x, y)) =
    case b of
        North -> robot { coordinates = (x, y + 1) }
        East  -> robot { coordinates = (x + 1, y) }
        South -> robot { coordinates = (x, y - 1) }
        West  -> robot { coordinates = (x - 1, y) }

-- Function to process a string of instructions
move :: Robot -> String -> Robot
move = foldl processInstruction
  where
    processInstruction :: Robot -> Char -> Robot
    processInstruction robot instruction =
        case instruction of
            'R' -> robot { bearing = turnRight (bearing robot) }
            'L' -> robot { bearing = turnLeft (bearing robot) }
            'A' -> advance robot
            _   -> robot -- Ignore any invalid instructions
