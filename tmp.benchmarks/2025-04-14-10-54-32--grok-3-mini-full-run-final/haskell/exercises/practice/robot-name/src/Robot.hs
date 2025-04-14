module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State (StateT)
import System.Random (randomR, newStdGen)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Char (chr, ord)  -- For character manipulation, though not directly used here

-- Global IORef to track used names for uniqueness
usedNames :: IORef [String]
usedNames = unsafePerformIO (newIORef [])  -- Initialized with an empty list

data Robot = Robot (IORef (Maybe String))  -- Robot holds an IORef to its name (Maybe String for optional name)

data RunState = RunState  -- Dummy data type as per the exercise

initialState :: RunState
initialState = RunState  -- Return a dummy value

mkRobot :: StateT RunState IO Robot
mkRobot = do
    ref <- liftIO $ newIORef Nothing  -- Create IORef for the robot's name
    liftIO $ generateUniqueName ref   -- Generate and set a unique name
    return (Robot ref)

resetName :: Robot -> StateT RunState IO ()
resetName (Robot ref) = liftIO $ writeIORef ref Nothing  -- Reset the name to Nothing

robotName :: Robot -> IO String
robotName (Robot ref) = do
    maybeName <- readIORef ref  -- Read the current name
    case maybeName of
        Just name -> return name    -- If name exists, return it
        Nothing -> do               -- If no name, generate a new one
            generateUniqueName ref
            newName <- readIORef ref  -- Read the newly set name
            case newName of
                Just name -> return name  -- Return the new name
                Nothing -> error "Failed to generate name"  -- This shouldn't happen

-- Helper function to generate a unique random name
generateUniqueName :: IORef (Maybe String) -> IO ()
generateUniqueName ref = do
    used <- readIORef usedNames  -- Get the list of used names
    let loop = do
            name <- generateRandomName  -- Generate a random name
            if name `notElem` used  -- Check if it's unique
                then do
                    writeIORef usedNames (name : used)  -- Add to used names
                    writeIORef ref (Just name)          -- Set the robot's name
                else loop  -- If not unique, try again
    loop

-- Helper function to generate a single random name
generateRandomName :: IO String
generateRandomName = do
    gen <- newStdGen  -- Get a new random generator
    let (letter1, gen1) = randomR (ord 'A', ord 'Z') gen  -- Random uppercase letter
    let (letter2, gen2) = randomR (ord 'A', ord 'Z') gen1
    let (digit1, gen3) = randomR (ord '0', ord '9') gen2  -- Random digit
    let (digit2, gen4) = randomR (ord '0', ord '9') gen3
    let (digit3, _)    = randomR (ord '0', ord '9') gen4
    return [chr letter1, chr letter2, chr digit1, chr digit2, chr digit3]  -- Convert codes back to characters and form the string
