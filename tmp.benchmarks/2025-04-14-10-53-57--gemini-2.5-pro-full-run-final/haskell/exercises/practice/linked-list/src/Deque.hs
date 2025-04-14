module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Control.Monad (when)
import Data.IORef
import Prelude hiding (head, tail)

-- Node in the doubly linked list
data Node a = Node
  { value :: a,
    prev :: IORef (Maybe (IORef (Node a))),
    next :: IORef (Maybe (IORef (Node a)))
  }

-- Deque structure holding references to head and tail nodes
data Deque a = Deque
  { head :: IORef (Maybe (IORef (Node a))),
    tail :: IORef (Maybe (IORef (Node a)))
  }

-- Creates a new empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  return $ Deque h t

-- Helper to create a new node reference
newNode :: a -> IO (IORef (Node a))
newNode val = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  newIORef (Node val p n)

-- Adds an element to the end (tail) of the Deque
push :: Deque a -> a -> IO ()
push deque x = do
  newNodeRef <- newNode x
  newNodeVal <- readIORef newNodeRef -- Read the Node value
  currentTailRef <- readIORef (tail deque)
  case currentTailRef of
    Nothing -> do
      -- Deque is empty
      writeIORef (head deque) (Just newNodeRef)
      writeIORef (tail deque) (Just newNodeRef)
    Just oldTailNodeRef -> do
      -- Deque is not empty
      oldTailNode <- readIORef oldTailNodeRef
      -- Link new node's prev to old tail
      writeIORef (prev newNodeVal) (Just oldTailNodeRef) -- Use prev newNodeVal
      -- Link old tail's next to new node
      writeIORef (next oldTailNode) (Just newNodeRef)
      -- Update deque's tail pointer
      writeIORef (tail deque) (Just newNodeRef)

-- Removes an element from the end (tail) of the Deque
pop :: Deque a -> IO (Maybe a)
pop deque = do
  currentTailRef <- readIORef (tail deque)
  case currentTailRef of
    Nothing -> return Nothing -- Deque is empty
    Just tailNodeRef -> do
      tailNode <- readIORef tailNodeRef
      let val = value tailNode
      prevNodeRef <- readIORef (prev tailNode)
      -- Update deque's tail to the previous node
      writeIORef (tail deque) prevNodeRef
      case prevNodeRef of
        Nothing ->
          -- Deque becomes empty
          writeIORef (head deque) Nothing
        Just newTailNodeRef -> do
          -- Deque still has elements, update new tail's next pointer
          newTailNode <- readIORef newTailNodeRef
          writeIORef (next newTailNode) Nothing
      return (Just val)

-- Adds an element to the beginning (head) of the Deque
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  newNodeRef <- newNode x
  newNodeVal <- readIORef newNodeRef -- Read the Node value
  currentHeadRef <- readIORef (head deque)
  case currentHeadRef of
    Nothing -> do
      -- Deque is empty
      writeIORef (head deque) (Just newNodeRef)
      writeIORef (tail deque) (Just newNodeRef)
    Just oldHeadNodeRef -> do
      -- Deque is not empty
      oldHeadNode <- readIORef oldHeadNodeRef
      -- Link new node's next to old head
      writeIORef (next newNodeVal) (Just oldHeadNodeRef) -- Use next newNodeVal
      -- Link old head's prev to new node
      writeIORef (prev oldHeadNode) (Just newNodeRef)
      -- Update deque's head pointer
      writeIORef (head deque) (Just newNodeRef)

-- Removes an element from the beginning (head) of the Deque
shift :: Deque a -> IO (Maybe a)
shift deque = do
  currentHeadRef <- readIORef (head deque)
  case currentHeadRef of
    Nothing -> return Nothing -- Deque is empty
    Just headNodeRef -> do
      headNode <- readIORef headNodeRef
      let val = value headNode
      nextNodeRef <- readIORef (next headNode)
      -- Update deque's head to the next node
      writeIORef (head deque) nextNodeRef
      case nextNodeRef of
        Nothing ->
          -- Deque becomes empty
          writeIORef (tail deque) Nothing
        Just newHeadNodeRef -> do
          -- Deque still has elements, update new head's prev pointer
          newHeadNode <- readIORef newHeadNodeRef
          writeIORef (prev newHeadNode) Nothing
      return (Just val)
