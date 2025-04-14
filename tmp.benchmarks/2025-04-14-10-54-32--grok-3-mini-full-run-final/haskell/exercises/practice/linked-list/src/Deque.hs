module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Data.List (reverse)  -- Import reverse for handling empty lists

data Deque a = Deque (IORef ([a], [a]))  -- IORef holding (front list, rear list)

-- Create a new empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
    ref <- newIORef ([], [])  -- Initialize with empty front and rear lists
    return (Deque ref)

-- Add an element to the end (rear) of the Deque
push :: Deque a -> a -> IO ()
push (Deque ref) x = do
    (front, rear) <- readIORef ref
    writeIORef ref (front, x : rear)  -- Prepend to rear list for efficiency in this implementation

-- Remove and return an element from the end (rear) of the Deque
pop :: Deque a -> IO (Maybe a)
pop (Deque ref) = do
    (front, rear) <- readIORef ref
    case rear of
        [] -> do  -- Rear is empty, so reverse front and use as rear
            let reversedFront = reverse front
            case reversedFront of
                [] -> return Nothing  -- Both are empty
                _ -> do
                    let (x:xs) = reversedFront  -- Take the last element after reversing
                    writeIORef ref ([], xs)  -- Update with empty front and the rest as rear (reversed)
                    return (Just x)
        (x:xs) -> do
            writeIORef ref (front, xs)  -- Update rear list
            return (Just x)

-- Add an element to the beginning (front) of the Deque
unshift :: Deque a -> a -> IO ()
unshift (Deque ref) x = do
    (front, rear) <- readIORef ref
    writeIORef ref (x : front, rear)  -- Prepend to front list

-- Remove and return an element from the beginning (front) of the Deque
shift :: Deque a -> IO (Maybe a)
shift (Deque ref) = do
    (front, rear) <- readIORef ref
    case front of
        [] -> do  -- Front is empty, so reverse rear and use as front
            let reversedRear = reverse rear
            case reversedRear of
                [] -> return Nothing  -- Both are empty
                (x:xs) -> do
                    writeIORef ref (xs, [])  -- Update with the rest as front and empty rear
                    return (Just x)
        (x:xs) -> do
            writeIORef ref (xs, rear)  -- Update front list
            return (Just x)
