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
mkRobot dir coords = Robot { robotBearing = dir, robotCoordinates = coords }

move :: Robot -> String -> Robot
move = foldl step
  where
    step :: Robot -> Char -> Robot
    step r 'R' = r { robotBearing = turnRight (robotBearing r) }
    step r 'L' = r { robotBearing = turnLeft (robotBearing r) }
    step r 'A' = r { robotCoordinates = advance (robotBearing r) (robotCoordinates r) }
    step r _   = r -- Ignore unknown instructions

    turnRight :: Bearing -> Bearing
    turnRight North = East
    turnRight East  = South
    turnRight South = West
    turnRight West  = North

    turnLeft :: Bearing -> Bearing
    turnLeft North = West
    turnLeft West  = South
    turnLeft South = East
    turnLeft East  = North

    advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
    advance North (x, y) = (x, y + 1)
    advance East  (x, y) = (x + 1, y)
    advance South (x, y) = (x, y - 1)
    advance West  (x, y) = (x - 1, y)
