module Robot
    ( Bearing (East, North, South, West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- | Cardinal directions a robot can face.
data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

-- | Internal representation of a robot, keeping its current bearing and
-- | its position on the infinite grid.
data Robot = Robot Bearing (Integer, Integer)
             deriving (Eq, Show)

-- | Retrieve the current bearing of the robot.
bearing :: Robot -> Bearing
bearing (Robot b _) = b

-- | Retrieve the current coordinates of the robot.
coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coords) = coords

-- | Construct a new robot with the given bearing and coordinates.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- | Apply a series of movement instructions to a robot, returning the
-- | new state of the robot.
--
-- Supported instructions (case–sensitive):
--   'R' – turn right  (clock‑wise)
--   'L' – turn left   (counter‑clock‑wise)
--   'A' – advance one unit in the direction the robot is currently facing
--
-- Any other character in the instruction string will be ignored.
move :: Robot -> String -> Robot
move = foldl applyInstruction
  where
    applyInstruction :: Robot -> Char -> Robot
    applyInstruction (Robot b (x, y)) instr =
      case instr of
        'R' -> Robot (turnRight b) (x, y)
        'L' -> Robot (turnLeft b)  (x, y)
        'A' ->
          let (nx, ny) = advance (x, y) b
          in Robot b (nx, ny)
        _   -> Robot b (x, y)  -- ignore unknown instructions

-- | Turn right (clock‑wise) from the current bearing.
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- | Turn left (counter‑clock‑wise) from the current bearing.
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- | Advance one unit in the given bearing.
advance :: (Integer, Integer) -> Bearing -> (Integer, Integer)
advance (x, y) North = (x    , y + 1)
advance (x, y) East  = (x + 1, y    )
advance (x, y) South = (x    , y - 1)
advance (x, y) West  = (x - 1, y    )
