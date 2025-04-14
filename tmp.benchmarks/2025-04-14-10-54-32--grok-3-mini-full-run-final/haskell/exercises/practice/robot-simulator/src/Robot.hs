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

data Robot = Robot { robotBearing :: Bearing, robotCoordinates :: (Integer, Integer) }

bearing :: Robot -> Bearing
bearing robot = robotBearing robot

coordinates :: Robot -> (Integer, Integer)
coordinates robot = robotCoordinates robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot { robotBearing = direction, robotCoordinates = coords }

move :: Robot -> String -> Robot
move robot instructions = foldl applyInstruction robot instructions
  where
    applyInstruction rob instr
      | instr == 'R' = rob { robotBearing = turnRight (robotBearing rob) }
      | instr == 'L' = rob { robotBearing = turnLeft (robotBearing rob) }
      | instr == 'A' = rob { robotCoordinates = advance (robotBearing rob) (robotCoordinates rob) }
      | otherwise    = rob  -- Ignore invalid instructions

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
    advance North (x, y) = (x, y + 1)  -- Move north: increase y
    advance East  (x, y) = (x + 1, y)  -- Move east: increase x
    advance South (x, y) = (x, y - 1)  -- Move south: decrease y
    advance West  (x, y) = (x - 1, y)  -- Move west: decrease x
