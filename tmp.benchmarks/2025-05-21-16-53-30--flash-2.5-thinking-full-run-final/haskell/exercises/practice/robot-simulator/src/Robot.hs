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
             deriving (Eq, Show)

-- Define the Robot data type to hold its bearing and coordinates
data Robot = Robot {
    robotBearing :: Bearing,
    robotCoordinates :: (Integer, Integer)
} deriving (Eq, Show)

-- Returns the bearing of the robot
bearing :: Robot -> Bearing
bearing robot = robotBearing robot

-- Returns the coordinates of the robot
coordinates :: Robot -> (Integer, Integer)
coordinates robot = robotCoordinates robot

-- Creates a new robot with the given direction and coordinates
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot direction coords

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
advance (Robot b (x, y))
    | b == North = Robot b (x, y + 1)
    | b == East  = Robot b (x + 1, y)
    | b == South = Robot b (x, y - 1)
    | b == West  = Robot b (x - 1, y)

-- Moves the robot according to a string of instructions
move :: Robot -> String -> Robot
move initialRobot instructions = foldl applyInstruction initialRobot instructions
  where
    applyInstruction :: Robot -> Char -> Robot
    applyInstruction robot 'R' = robot { robotBearing = turnRight (robotBearing robot) }
    applyInstruction robot 'L' = robot { robotBearing = turnLeft (robotBearing robot) }
    applyInstruction robot 'A' = advance robot
    applyInstruction robot _   = robot -- Handle unexpected characters gracefully, though instructions should be 'R', 'L', 'A'
