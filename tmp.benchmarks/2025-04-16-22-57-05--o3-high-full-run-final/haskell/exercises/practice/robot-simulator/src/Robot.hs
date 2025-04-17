module Robot
    ( Bearing (East, North, South, West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- | Cardinal directions a robot can face.
data Bearing
    = North
    | East
    | South
    | West
    deriving (Eq, Show)

-- | A robot knows its current bearing and its Cartesian coordinates.
data Robot = Robot
    { robotBearing     :: Bearing
    , robotCoordinates :: (Integer, Integer)
    } deriving (Eq, Show)

-- | Extract the robot's current bearing.
bearing :: Robot -> Bearing
bearing = robotBearing

-- | Extract the robot's current coordinates.
coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

-- | Construct a new robot given an initial bearing and coordinates.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- | Apply a series of instructions to a robot, returning its new state.
-- Recognised instructions are:
--   'R' – turn right  (clock‑wise)
--   'L' – turn left   (counter‑clock‑wise)
--   'A' – advance one grid unit in the current bearing
move :: Robot -> String -> Robot
move = foldl step
  where
    step :: Robot -> Char -> Robot
    step r 'R' = r { robotBearing = turnRight (bearing r) }
    step r 'L' = r { robotBearing = turnLeft  (bearing r) }
    step r 'A' = advance r
    -- Ignore any unrecognised instruction.
    step r _   = r

-- Internal helpers -----------------------------------------------------------

-- | One‑step movement forward in whichever direction the robot is facing.
advance :: Robot -> Robot
advance (Robot dir (x, y)) = case dir of
    North -> Robot dir (x    , y + 1)
    East  -> Robot dir (x + 1, y    )
    South -> Robot dir (x    , y - 1)
    West  -> Robot dir (x - 1, y    )

-- | Turn the given direction one step clockwise.
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- | Turn the given direction one step counter‑clockwise.
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North
