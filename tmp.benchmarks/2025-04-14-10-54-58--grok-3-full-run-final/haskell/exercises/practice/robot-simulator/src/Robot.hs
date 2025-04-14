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

data Robot = Robot { robotBearing :: Bearing
                   , robotCoords :: (Integer, Integer)
                   } deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot { robotBearing = direction, robotCoords = coords }

move :: Robot -> String -> Robot
move robot instructions = foldl applyInstruction robot instructions
  where
    applyInstruction r 'R' = r { robotBearing = turnRight (bearing r) }
    applyInstruction r 'L' = r { robotBearing = turnLeft (bearing r) }
    applyInstruction r 'A' = r { robotCoords = advance (bearing r) (coordinates r) }
    applyInstruction r _ = r

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
