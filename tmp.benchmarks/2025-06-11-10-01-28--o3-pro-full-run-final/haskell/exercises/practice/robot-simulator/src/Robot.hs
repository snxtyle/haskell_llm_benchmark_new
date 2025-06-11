module Robot
    ( Bearing(East, North, South, West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

-- | Cardinal points toward which the robot can face.
data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

-- | Internal representation of a robot: its current bearing and position.
data Robot = Robot
    { robotBearing     :: Bearing
    , robotCoordinates :: (Integer, Integer)
    } deriving (Eq, Show)

-- | Obtain the robot's current bearing.
bearing :: Robot -> Bearing
bearing = robotBearing

-- | Obtain the robot's current coordinates.
coordinates :: Robot -> (Integer, Integer)
coordinates = robotCoordinates

-- | Create a new robot at the supplied bearing and coordinates.
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- | Advance the robot one grid unit in the direction it is currently facing.
advance :: Robot -> Robot
advance (Robot dir (x, y)) =
    let newCoords = case dir of
            North -> (x    , y + 1)
            East  -> (x + 1, y    )
            South -> (x    , y - 1)
            West  -> (x - 1, y    )
    in Robot dir newCoords

-- | Turn the robot 90° to the right.
turnRight :: Robot -> Robot
turnRight (Robot dir coords) =
    let newDir = case dir of
            North -> East
            East  -> South
            South -> West
            West  -> North
    in Robot newDir coords

-- | Turn the robot 90° to the left.
turnLeft :: Robot -> Robot
turnLeft (Robot dir coords) =
    let newDir = case dir of
            North -> West
            West  -> South
            South -> East
            East  -> North
    in Robot newDir coords

-- | Apply a sequence of instructions to a robot.
--   'R' = turn right, 'L' = turn left, 'A' = advance.
move :: Robot -> String -> Robot
move = foldl step
  where
    step :: Robot -> Char -> Robot
    step r 'R' = turnRight r
    step r 'L' = turnLeft r
    step r 'A' = advance r
    step _  c  = error $ "Invalid instruction: " ++ show c
