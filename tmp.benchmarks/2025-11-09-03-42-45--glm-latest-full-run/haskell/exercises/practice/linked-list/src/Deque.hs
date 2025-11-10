module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Internal type for a reference to a node.
type NodeRef a = IORef (Node a)

-- Internal node structure for the doubly linked list.
data Node a = Node
  { _value :: a
  , _prev :: IORef (Maybe (NodeRef a))
  , _next :: IORef (Maybe (NodeRef a))
  }

-- The Deque itself, holding references to the head and tail of the list.
newtype Deque a = Deque (IORef (Maybe (NodeRef a), Maybe (NodeRef a)))

-- | Create a new, empty Deque.
mkDeque :: IO (Deque a)
mkDeque = do
  ref <- newIORef (Nothing, Nothing)
  return (Deque ref)

-- | Add an element to the end (tail) of the Deque.
push :: Deque a -> a -> IO ()
push (Deque dequeRef) x = do
  (head, tail) <- readIORef dequeRef
  -- Create the new node. Its 'prev' points to the current tail.
  prevRef <- newIORef tail
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  newNodeRef <- newIORef newNode

  case tail of
    -- Deque was empty, new node is both head and tail.
    Nothing -> writeIORef dequeRef (Just newNodeRef, Just newNodeRef)
    -- Deque was not empty, link old tail to new node.
    Just oldTailRef -> do
      oldTail <- readIORef oldTailRef
      writeIORef (_next oldTail) (Just newNodeRef)
      writeIORef dequeRef (head, Just newNodeRef)

-- | Remove and return an element from the end (tail) of the Deque.
pop :: Deque a -> IO (Maybe a)
pop (Deque dequeRef) = do
  (head, tail) <- readIORef dequeRef
  case tail of
    Nothing -> return Nothing -- Deque is empty.
    Just tailRef -> do
      tailNode <- readIORef tailRef
      let val = _value tailNode
      newTailRef <- readIORef (_prev tailNode)
      case newTailRef of
        -- This was the last element, deque is now empty.
        Nothing -> writeIORef dequeRef (Nothing, Nothing)
        -- There are more elements, update the new tail's 'next' pointer.
        Just newTail -> do
          newTailNode <- readIORef newTail
          writeIORef (_next newTailNode) Nothing
          writeIORef dequeRef (head, Just newTail)
      return (Just val)

-- | Add an element to the beginning (head) of the Deque.
unshift :: Deque a -> a -> IO ()
unshift (Deque dequeRef) x = do
  (head, tail) <- readIORef dequeRef
  -- Create the new node. Its 'next' points to the current head.
  prevRef <- newIORef Nothing
  nextRef <- newIORef head
  let newNode = Node x prevRef nextRef
  newNodeRef <- newIORef newNode

  case head of
    -- Deque was empty, new node is both head and tail.
    Nothing -> writeIORef dequeRef (Just newNodeRef, Just newNodeRef)
    -- Deque was not empty, link old head to new node.
    Just oldHeadRef -> do
      oldHead <- readIORef oldHeadRef
      writeIORef (_prev oldHead) (Just newNodeRef)
      writeIORef dequeRef (Just newNodeRef, tail)

-- | Remove and return an element from the beginning (head) of the Deque.
shift :: Deque a -> IO (Maybe a)
shift (Deque dequeRef) = do
  (head, tail) <- readIORef dequeRef
  case head of
    Nothing -> return Nothing -- Deque is empty.
    Just headRef -> do
      headNode <- readIORef headRef
      let val = _value headNode
      newHeadRef <- readIORef (_next headNode)
      case newHeadRef of
        -- This was the last element, deque is now empty.
        Nothing -> writeIORef dequeRef (Nothing, Nothing)
        -- There are more elements, update the new head's 'prev' pointer.
        Just newHead -> do
          newHeadNode <- readIORef newHead
          writeIORef (_prev newHeadNode) Nothing
          writeIORef dequeRef (Just newHead, tail)
      return (Just val)
