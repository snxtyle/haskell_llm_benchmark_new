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

-- A Robot holds its current bearing and (x, y) coordinates
data Robot = Robot Bearing (Integer, Integer)
    deriving (Eq, Show)

-- Extract the bearing
bearing :: Robot -> Bearing
bearing (Robot b _) = b

-- Extract the coordinates
coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ pos) = pos

-- Create a robot with the given bearing and coordinates
mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

-- Process a stream of instructions
move :: Robot -> String -> Robot
move = foldl applyInstruction

-- Apply a single instruction to the robot
applyInstruction :: Robot -> Char -> Robot
applyInstruction (Robot b pos) instr =
    case instr of
        'L' -> Robot (turnLeft b) pos
        'R' -> Robot (turnRight b) pos
        'A' -> Robot b (advance b pos)
        _   -> error $ "Invalid instruction: " ++ [instr]

-- Turn the bearing 90° left
turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

-- Turn the bearing 90° right
turnRight :: Bearing -> Bearing
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

-- Move one step forward in the given bearing
advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance North (x, y) = (x,     y + 1)
advance South (x, y) = (x,     y - 1)
advance East  (x, y) = (x + 1, y)
advance West  (x, y) = (x - 1, y)
