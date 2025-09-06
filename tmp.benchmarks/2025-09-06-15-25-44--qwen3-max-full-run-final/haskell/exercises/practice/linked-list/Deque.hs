module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- A node in the doubly linked list
data Node a = Node
  { value :: a
  , prev :: Maybe (IORef (Node a))
  , next :: Maybe (IORef (Node a))
  }

-- The Deque represents a doubly linked list with references to head and tail
data Deque a = Deque
  { headRef :: IORef (Maybe (IORef (Node a)))
  , tailRef :: IORef (Maybe (IORef (Node a)))
  , sizeRef :: IORef Int
  }

-- Create a new empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  headRef' <- newIORef Nothing
  tailRef' <- newIORef Nothing
  sizeRef' <- newIORef 0
  return $ Deque headRef' tailRef' sizeRef'

-- Remove and return the last element (from the tail)
pop :: Deque a -> IO (Maybe a)
pop deque = do
  mTail <- readIORef (tailRef deque)
  case mTail of
    Nothing -> return Nothing
    Just tailNodeRef -> do
      tailNode <- readIORef tailNodeRef
      let prevNode = prev tailNode
      
      -- Update the tail reference
      writeIORef (tailRef deque) prevNode
      
      -- If there was a previous node, set its next to Nothing
      case prevNode of
        Just prevRef -> do
          prevNode' <- readIORef prevRef
          writeIORef prevRef (prevNode' { next = Nothing })
        Nothing -> -- This was the only node, so head should also be Nothing
          writeIORef (headRef deque) Nothing
      
      -- Decrement size
      modifyIORef (sizeRef deque) (\s -> s - 1)
      
      return $ Just (value tailNode)

-- Add an element to the end (tail)
push :: Deque a -> a -> IO ()
push deque x = do
  mTail <- readIORef (tailRef deque)
  newNodeRef <- newIORef (Node x mTail Nothing)
  
  case mTail of
    Nothing -> -- Empty deque, this is the first node
      writeIORef (headRef deque) (Just newNodeRef)
    Just tailRef' -> do -- Update the current tail's next pointer
      tailNode <- readIORef tailRef'
      writeIORef tailRef' (tailNode { next = Just newNodeRef })
  
  -- Update the tail reference to point to the new node
  writeIORef (tailRef deque) (Just newNodeRef)
  
  -- Increment size
  modifyIORef (sizeRef deque) (+1)

-- Add an element to the beginning (head)
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  mHead <- readIORef (headRef deque)
  newNodeRef <- newIORef (Node x Nothing mHead)
  
  case mHead of
    Nothing -> -- Empty deque, this is the first node
      writeIORef (tailRef deque) (Just newNodeRef)
    Just headRef' -> do -- Update the current head's prev pointer
      headNode <- readIORef headRef'
      writeIORef headRef' (headNode { prev = Just newNodeRef })
  
  -- Update the head reference to point to the new node
  writeIORef (headRef deque) (Just newNodeRef)
  
  -- Increment size
  modifyIORef (sizeRef deque) (+1)

-- Remove and return the first element (from the head)
shift :: Deque a -> IO (Maybe a)
shift deque = do
  mHead <- readIORef (headRef deque)
  case mHead of
    Nothing -> return Nothing
    Just headNodeRef -> do
      headNode <- readIORef headNodeRef
      let nextNode = next headNode
      
      -- Update the head reference
      writeIORef (headRef deque) nextNode
      
      -- If there was a next node, set its prev to Nothing
      case nextNode of
        Just nextRef -> do
          nextNode' <- readIORef nextRef
          writeIORef nextRef (nextNode' { prev = Nothing })
        Nothing -> -- This was the only node, so tail should also be Nothing
          writeIORef (tailRef deque) Nothing
      
      -- Decrement size
      modifyIORef (sizeRef deque) (\s -> s - 1)
      
      return $ Just (value headNode)
