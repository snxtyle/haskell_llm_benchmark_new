module Robot
    ( Bearing(East,North,South,West)
    , Robot
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
  { bearing :: Bearing
  , coordinates :: (Integer, Integer)
  }
  deriving (Eq, Show)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

move :: Robot -> String -> Robot
move robot instructions = foldl step robot instructions
  where
    step r 'R' = r { bearing = turnRight (bearing r) }
    step r 'L' = r { bearing = turnLeft (bearing r) }
    step r 'A' = r { coordinates = advance (coordinates r) (bearing r) }
    step r _   = r

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

advance :: (Integer, Integer) -> Bearing -> (Integer, Integer)
advance (x,y) North = (x, y+1)
advance (x,y) East  = (x+1, y)
advance (x,y) South = (x, y-1)
advance (x,y) West  = (x-1, y)
