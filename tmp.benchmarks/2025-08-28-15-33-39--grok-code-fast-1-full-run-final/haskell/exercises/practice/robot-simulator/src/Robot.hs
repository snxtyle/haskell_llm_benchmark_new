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

data Robot = Robot { robotBearing :: Bearing, robotPosition :: (Integer, Integer) }

bearing :: Robot -> Bearing
bearing = robotBearing

coordinates :: Robot -> (Integer, Integer)
coordinates = robotPosition

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

advance :: Robot -> Robot
advance (Robot b (x, y)) = case b of
    North -> Robot b (x, y + 1)
    East -> Robot b (x + 1, y)
    South -> Robot b (x, y - 1)
    West -> Robot b (x - 1, y)

applyInstruction :: Robot -> Char -> Robot
applyInstruction r 'R' = r { robotBearing = turnRight (robotBearing r) }
applyInstruction r 'L' = r { robotBearing = turnLeft (robotBearing r) }
applyInstruction r 'A' = advance r
applyInstruction r _ = r

move :: Robot -> String -> Robot
move robot instructions = foldl applyInstruction robot instructions
