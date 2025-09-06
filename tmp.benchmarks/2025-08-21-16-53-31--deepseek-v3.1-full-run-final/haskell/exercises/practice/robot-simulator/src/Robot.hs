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

data Robot = Robot 
    { robotBearing :: Bearing
    , robotCoordinates :: (Integer, Integer)
    } deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot direction coords

move :: Robot -> String -> Robot
move robot [] = robot
move robot (instruction:instructions) = 
    move (processInstruction robot instruction) instructions
    where
        processInstruction r 'R' = r { robotBearing = turnRight (robotBearing r) }
        processInstruction r 'L' = r { robotBearing = turnLeft (robotBearing r) }
        processInstruction r 'A' = r { robotCoordinates = advance (robotBearing r) (robotCoordinates r) }
        processInstruction r _   = r  -- ignore invalid instructions
        
        turnRight North = East
        turnRight East = South
        turnRight South = West
        turnRight West = North
        
        turnLeft North = West
        turnLeft West = South
        turnLeft South = East
        turnLeft East = North
        
        advance North (x, y) = (x, y + 1)
        advance East (x, y) = (x + 1, y)
        advance South (x, y) = (x, y - 1)
        advance West (x, y) = (x - 1, y)
