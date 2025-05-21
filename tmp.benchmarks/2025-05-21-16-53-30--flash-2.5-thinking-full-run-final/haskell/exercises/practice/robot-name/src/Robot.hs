module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT, get, put, liftIO, State, runState) -- Added State, runState
import System.Random (StdGen, mkStdGen, randomR)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Control.Monad (replicateM) -- replicateM is not used, but not an error
import Data.Char (chr)

-- | Represents the global state of the robot factory.
-- It keeps track of all names that have been assigned to robots
-- to ensure uniqueness, and the random number generator.
data RunState = RunState
    { usedNames :: Set.Set String
    , generator :: StdGen
    }

-- | Represents a single robot.
-- It holds a mutable reference to its current name, which can be Nothing
-- if the name has been wiped, or Just a String if it has a name.
newtype Robot = Robot (IORef (Maybe String))

-- | The initial state of the robot factory.
-- The set of used names is empty, and the random generator is initialized
-- with a fixed seed for reproducibility in a pure context.
initialState :: RunState
initialState = RunState
    { usedNames = Set.empty
    , generator = mkStdGen 0 -- Using a fixed seed for initial state
    }

-- | Helper function to generate a single random character within a range.
-- It operates within the State monad to update the StdGen.
randomCharS :: Char -> Char -> State StdGen Char -- Changed from StateT StdGen IO Char
randomCharS lower upper = do
    g <- get
    let (r, g') = randomR (fromEnum lower, fromEnum upper) g
    put g'
    return (chr r)

-- | Helper function to generate a random robot name (e.g., "AB123").
-- It operates within the StateT RunState IO monad to get and update the StdGen.
generateRandomName :: StateT RunState IO String
generateRandomName = do
    s <- get
    let currentGen = generator s
    -- Use a local State monad to generate the name and update the generator
    let (name, nextGen) = flip runState currentGen $ do
            c1 <- randomCharS 'A' 'Z'
            c2 <- randomCharS 'A' 'Z'
            d1 <- randomCharS '0' '9'
            d2 <- randomCharS '0' '9'
            d3 <- randomCharS '0' '9'
            return ([c1, c2, d1, d2, d3])
    put s { generator = nextGen } -- Update the global generator
    return name

-- | Generates a unique robot name.
-- It repeatedly generates names until a unique one is found,
-- then adds it to the set of used names in the RunState.
getUniqueName :: StateT RunState IO String
getUniqueName = do
    name <- generateRandomName
    s <- get
    if Set.member name (usedNames s)
        then getUniqueName -- Name already used, try again
        else do
            put s { usedNames = Set.insert name (usedNames s) }
            return name

-- | Creates a new robot.
-- A newly created robot has no name initially.
mkRobot :: StateT RunState IO Robot
mkRobot = liftIO $ Robot <$> newIORef Nothing

-- | Resets a robot to its factory settings, wiping its name.
-- The next time its name is requested, a new unique name will be generated.
resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = liftIO $ writeIORef ref Nothing

-- | Returns the robot's current name.
-- If the robot has no name (e.g., just created or reset), a new unique
-- name is generated, assigned to the robot, and returned.
--
-- NOTE: The original type signature was `Robot -> IO String`.
-- To correctly manage unique names and the random generator within the
-- global `RunState`, this function must operate within the `StateT RunState IO` monad.
-- This change allows it to access and modify the global state.
robotName :: Robot -> StateT RunState IO String
robotName (Robot ref) = do
    mName <- liftIO $ readIORef ref
    case mName of
        Just name -> return name
        Nothing -> do
            newName <- getUniqueName
            liftIO $ writeIORef ref (Just newName)
            return newName
