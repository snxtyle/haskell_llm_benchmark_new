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
    deriving (Eq, Show)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot b c = Robot b c

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

move :: Robot -> String -> Robot
move = foldl step
  where
    step (Robot b c) instruction = case instruction of
      'R' -> Robot (turnRight b) c
      'L' -> Robot (turnLeft b) c
      'A' -> Robot b (advance b c)
      _   -> Robot b c  -- ignore unknown instructions
