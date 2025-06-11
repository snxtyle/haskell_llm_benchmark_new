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

data Robot = Robot { bearing :: Bearing, coordinates :: (Integer, Integer) }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

advance :: Robot -> Robot
advance robot@(Robot { bearing = b, coordinates = (x, y) }) =
    case b of
        North -> robot { coordinates = (x, y + 1) }
        East  -> robot { coordinates = (x + 1, y) }
        South -> robot { coordinates = (x, y - 1) }
        West  -> robot { coordinates = (x - 1, y) }

move :: Robot -> String -> Robot
move = foldl step
  where
    step robot 'L' = robot { bearing = turnLeft (bearing robot) }
    step robot 'R' = robot { bearing = turnRight (bearing robot) }
    step robot 'A' = advance robot
    step robot _   = robot
