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
move robot instructions = foldl processInstruction robot instructions
  where
    processInstruction :: Robot -> Char -> Robot
    processInstruction r 'R' = turnRight r
    processInstruction r 'L' = turnLeft r
    processInstruction r 'A' = advance r
    processInstruction r _   = r  -- ignore unknown instructions

    turnRight :: Robot -> Robot
    turnRight r = r { robotBearing = nextClockwise (robotBearing r) }
      where
        nextClockwise North = East
        nextClockwise East  = South
        nextClockwise South = West
        nextClockwise West  = North

    turnLeft :: Robot -> Robot
    turnLeft r = r { robotBearing = nextCounterClockwise (robotBearing r) }
      where
        nextCounterClockwise North = West
        nextCounterClockwise West  = South
        nextCounterClockwise South = East
        nextCounterClockwise East  = North

    advance :: Robot -> Robot
    advance r = r { robotCoordinates = newCoords }
      where
        (x, y) = robotCoordinates r
        newCoords = case robotBearing r of
          North -> (x, y + 1)
          East  -> (x + 1, y)
          South -> (x, y - 1)
          West  -> (x - 1, y)
