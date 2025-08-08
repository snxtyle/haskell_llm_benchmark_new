module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Node in the doubly-linked list
data Node a = Node
  { value :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }

-- Deque holds references to the first and last nodes
data Deque a = Deque
  { firstNode :: IORef (Maybe (Node a))
  , lastNode :: IORef (Maybe (Node a))
  }

-- Create an empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  first <- newIORef Nothing
  last' <- newIORef Nothing
  return $ Deque first last'

-- Remove and return the last element
pop :: Deque a -> IO (Maybe a)
pop deque = do
  lastMaybe <- readIORef (lastNode deque)
  case lastMaybe of
    Nothing -> return Nothing
    Just node -> do
      prevMaybe <- readIORef (prev node)
      case prevMaybe of
        Nothing -> do
          -- This was the only node
          writeIORef (firstNode deque) Nothing
          writeIORef (lastNode deque) Nothing
        Just prevNode -> do
          -- Update the previous node to be the new last
          writeIORef (next prevNode) Nothing
          writeIORef (lastNode deque) (Just prevNode)
      return $ Just (value node)

-- Add an element to the end
push :: Deque a -> a -> IO ()
push deque x = do
  lastMaybe <- readIORef (lastNode deque)
  prevRef <- newIORef lastMaybe
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  
  case lastMaybe of
    Nothing -> do
      -- First node in the deque
      writeIORef (firstNode deque) (Just newNode)
      writeIORef (lastNode deque) (Just newNode)
    Just lastNode' -> do
      -- Add to the end
      writeIORef (next lastNode') (Just newNode)
      writeIORef (lastNode deque) (Just newNode)

-- Add an element to the beginning
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  firstMaybe <- readIORef (firstNode deque)
  prevRef <- newIORef Nothing
  nextRef <- newIORef firstMaybe
  let newNode = Node x prevRef nextRef
  
  case firstMaybe of
    Nothing -> do
      -- First node in the deque
      writeIORef (firstNode deque) (Just newNode)
      writeIORef (lastNode deque) (Just newNode)
    Just firstNode' -> do
      -- Add to the beginning
      writeIORef (prev firstNode') (Just newNode)
      writeIORef (firstNode deque) (Just newNode)

-- Remove and return the first element
shift :: Deque a -> IO (Maybe a)
shift deque = do
  firstMaybe <- readIORef (firstNode deque)
  case firstMaybe of
    Nothing -> return Nothing
    Just node -> do
      nextMaybe <- readIORef (next node)
      case nextMaybe of
        Nothing -> do
          -- This was the only node
          writeIORef (firstNode deque) Nothing
          writeIORef (lastNode deque) Nothing
        Just nextNode -> do
          -- Update the next node to be the new first
          writeIORef (prev nextNode) Nothing
          writeIORef (firstNode deque) (Just nextNode)
      return $ Just (value node)
