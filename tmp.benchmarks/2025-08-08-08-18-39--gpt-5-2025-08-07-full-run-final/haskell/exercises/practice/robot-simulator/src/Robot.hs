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
coordinates (Robot _ pos) = pos

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot dir pos = Robot dir pos

move :: Robot -> String -> Robot
move = foldl step
  where
    step :: Robot -> Char -> Robot
    step r@(Robot b (x, y)) instr =
      case instr of
        'R' -> Robot (turnRight b) (x, y)
        'L' -> Robot (turnLeft b) (x, y)
        'A' -> case b of
                 North -> Robot b (x, y + 1)
                 East  -> Robot b (x + 1, y)
                 South -> Robot b (x, y - 1)
                 West  -> Robot b (x - 1, y)
        _   -> r  -- Ignore unrecognized instructions

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
