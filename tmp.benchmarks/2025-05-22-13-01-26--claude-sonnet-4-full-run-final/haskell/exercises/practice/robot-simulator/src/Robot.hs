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

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coords) = coords

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coords = Robot direction coords

move :: Robot -> String -> Robot
move robot instructions = foldl processInstruction robot instructions
  where
    processInstruction r 'R' = turnRight r
    processInstruction r 'L' = turnLeft r
    processInstruction r 'A' = advance r
    processInstruction r _   = r  -- ignore invalid instructions

turnRight :: Robot -> Robot
turnRight (Robot North coords) = Robot East coords
turnRight (Robot East coords)  = Robot South coords
turnRight (Robot South coords) = Robot West coords
turnRight (Robot West coords)  = Robot North coords

turnLeft :: Robot -> Robot
turnLeft (Robot North coords) = Robot West coords
turnLeft (Robot West coords)  = Robot South coords
turnLeft (Robot South coords) = Robot East coords
turnLeft (Robot East coords)  = Robot North coords

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y + 1)
advance (Robot East (x, y))  = Robot East (x + 1, y)
advance (Robot South (x, y)) = Robot South (x, y - 1)
advance (Robot West (x, y))  = Robot West (x - 1, y)
