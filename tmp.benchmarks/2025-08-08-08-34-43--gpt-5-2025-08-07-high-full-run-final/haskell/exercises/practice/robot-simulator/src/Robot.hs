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
    { bearing     :: Bearing
    , coordinates :: (Integer, Integer)
    } deriving (Eq, Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir coords = Robot { bearing = dir, coordinates = coords }

move :: Robot -> String -> Robot
move = foldl step
  where
    step r 'R' = r { bearing = turnRight (bearing r) }
    step r 'L' = r { bearing = turnLeft  (bearing r) }
    step r 'A' = r { coordinates = advance (bearing r) (coordinates r) }
    step r _   = r  -- ignore unknown instructions

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
