module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

import Data.List (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { _bearing :: Bearing
                   , _coordinates :: (Integer, Integer)
                   } deriving (Show)

-- | Returns the robot's current bearing.
bearing :: Robot -> Bearing
bearing = _bearing

-- | Returns the robot's current coordinates.
coordinates :: Robot -> (Integer, Integer)
coordinates = _coordinates

-- | Creates a new robot with a given bearing and coordinates.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- | Turns the robot to the right.
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- | Turns the robot to the left.
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- | Moves the robot forward one unit in the direction it is facing.
advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x, y) = (x, y + 1)
advance East  (x, y) = (x + 1, y)
advance South (x, y) = (x, y - 1)
advance West  (x, y) = (x - 1, y)

-- | Applies a single instruction to a robot.
applyInstruction :: Robot -> Char -> Robot
applyInstruction robot 'R' = robot { _bearing = turnRight (_bearing robot) }
applyInstruction robot 'L' = robot { _bearing = turnLeft (_bearing robot) }
applyInstruction robot 'A' = robot { _coordinates = advance (_bearing robot) (_coordinates robot) }
applyInstruction robot _   = robot -- Ignores any other characters

-- | Simulates the robot's movement based on a string of instructions.
move :: Robot -> String -> Robot
move robot instructions = foldl' applyInstruction robot instructions
