module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Node in the doubly-linked list
data Node a = Node
  { value :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }

-- Deque with references to head and tail
data Deque a = Deque
  { headRef :: IORef (Maybe (Node a))
  , tailRef :: IORef (Maybe (Node a))
  }

-- Create a new empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  return $ Deque h t

-- Remove and return the last element
pop :: Deque a -> IO (Maybe a)
pop deque = do
  tailNode <- readIORef (tailRef deque)
  case tailNode of
    Nothing -> return Nothing
    Just node -> do
      prevNode <- readIORef (prev node)
      writeIORef (tailRef deque) prevNode
      case prevNode of
        Nothing -> writeIORef (headRef deque) Nothing
        Just pNode -> writeIORef (next pNode) Nothing
      return $ Just (value node)

-- Add an element to the end
push :: Deque a -> a -> IO ()
push deque x = do
  tailNode <- readIORef (tailRef deque)
  prevRef <- newIORef tailNode
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  
  case tailNode of
    Nothing -> do
      -- Empty deque
      writeIORef (headRef deque) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)
    Just tNode -> do
      -- Non-empty deque
      writeIORef (next tNode) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)

-- Add an element to the beginning
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  headNode <- readIORef (headRef deque)
  prevRef <- newIORef Nothing
  nextRef <- newIORef headNode
  let newNode = Node x prevRef nextRef
  
  case headNode of
    Nothing -> do
      -- Empty deque
      writeIORef (headRef deque) (Just newNode)
      writeIORef (tailRef deque) (Just newNode)
    Just hNode -> do
      -- Non-empty deque
      writeIORef (prev hNode) (Just newNode)
      writeIORef (headRef deque) (Just newNode)

-- Remove and return the first element
shift :: Deque a -> IO (Maybe a)
shift deque = do
  headNode <- readIORef (headRef deque)
  case headNode of
    Nothing -> return Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      writeIORef (headRef deque) nextNode
      case nextNode of
        Nothing -> writeIORef (tailRef deque) Nothing
        Just nNode -> writeIORef (prev nNode) Nothing
      return $ Just (value node)
