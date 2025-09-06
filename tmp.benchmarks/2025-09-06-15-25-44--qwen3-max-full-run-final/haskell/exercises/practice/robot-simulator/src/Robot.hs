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

-- Define Robot as a data type with bearing and coordinates
data Robot = Robot Bearing (Integer, Integer)
             deriving (Eq, Show)

-- Get the bearing of the robot
bearing :: Robot -> Bearing
bearing (Robot b _) = b

-- Get the coordinates of the robot
coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

-- Create a robot with given bearing and coordinates
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- Move the robot according to instructions
move :: Robot -> String -> Robot
move robot [] = robot
move robot (instruction:instructions) = 
    let nextRobot = case instruction of
            'R' -> turnRight robot
            'L' -> turnLeft robot
            'A' -> advance robot
            _ -> robot  -- Ignore invalid instructions
    in move nextRobot instructions

-- Helper function to turn right
turnRight :: Robot -> Robot
turnRight (Robot b c) = Robot (case b of
    North -> East
    East -> South
    South -> West
    West -> North) c

-- Helper function to turn left
turnLeft :: Robot -> Robot
turnLeft (Robot b c) = Robot (case b of
    North -> West
    West -> South
    South -> East
    East -> North) c

-- Helper function to advance
advance :: Robot -> Robot
advance (Robot b (x, y)) = Robot b (case b of
    North -> (x, y + 1)
    East -> (x + 1, y)
    South -> (x, y - 1)
    West -> (x - 1, y))
