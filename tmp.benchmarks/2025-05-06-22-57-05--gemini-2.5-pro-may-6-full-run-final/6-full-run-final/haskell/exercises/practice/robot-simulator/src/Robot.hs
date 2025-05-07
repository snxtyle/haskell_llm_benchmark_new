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

-- Define the Robot data type to store its bearing and coordinates
data Robot = Robot Bearing (Integer, Integer) deriving (Eq, Show)

-- bearing: Returns the current bearing of the robot.
bearing :: Robot -> Bearing
bearing (Robot b _) = b

-- coordinates: Returns the current (x, y) coordinates of the robot.
coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

-- mkRobot: Creates a new robot with a given bearing and initial coordinates.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot -- This is equivalent to: mkRobot initialBearing initialCoordinates = Robot initialBearing initialCoordinates

-- Helper function to turn the robot right.
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- Helper function to turn the robot left.
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- Helper function to calculate new coordinates after advancing.
advanceCoords :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advanceCoords North (x, y) = (x, y + 1)
advanceCoords East  (x, y) = (x + 1, y)
advanceCoords South (x, y) = (x, y - 1)
advanceCoords West  (x, y) = (x - 1, y)

-- Helper function to process a single instruction character.
processInstruction :: Robot -> Char -> Robot
processInstruction (Robot currentBearing currentCoords) instruction =
    case instruction of
        'R' -> Robot (turnRight currentBearing) currentCoords
        'L' -> Robot (turnLeft currentBearing) currentCoords
        'A' -> Robot currentBearing (advanceCoords currentBearing currentCoords)
        _   -> Robot currentBearing currentCoords -- Ignore any unknown instructions

-- move: Processes a string of instructions to move the robot.
move :: Robot -> String -> Robot
move initialRobot instructions = foldl processInstruction initialRobot instructions
