module Deque
  ( Deque
  , mkDeque
  , pop
  , push
  , shift
  , unshift
  ) where

import Data.IORef

-- | Internal node in the doubly–linked list.
data Node a = Node
  { nodeValue :: a
  , nodePrev  :: IORef (Maybe (Node a))
  , nodeNext  :: IORef (Maybe (Node a))
  }

-- Helper to build a fresh node.
newNode :: a -> Maybe (Node a) -> Maybe (Node a) -> IO (Node a)
newNode v prev next = do
  prevRef <- newIORef prev
  nextRef <- newIORef next
  pure (Node v prevRef nextRef)

-- | Public deque type.  Holds references to both ends of the list.
data Deque a = Deque
  { dqHead :: IORef (Maybe (Node a))
  , dqTail :: IORef (Maybe (Node a))
  }

-- | Create an empty deque.
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  pure (Deque h t)

-- | Add an element to the back of the deque.
push :: Deque a -> a -> IO ()
push deque x = do
  mTail <- readIORef (dqTail deque)
  node  <- newNode x mTail Nothing
  case mTail of
    Nothing        -> writeIORef (dqHead deque) (Just node)
    Just tailNode  -> writeIORef (nodeNext tailNode) (Just node)
  writeIORef (dqTail deque) (Just node)

-- | Add an element to the front of the deque.
unshift :: Deque a -> a -> IO ()
unshift deque x = do
  mHead <- readIORef (dqHead deque)
  node  <- newNode x Nothing mHead
  case mHead of
    Nothing        -> writeIORef (dqTail deque) (Just node)
    Just headNode  -> writeIORef (nodePrev headNode) (Just node)
  writeIORef (dqHead deque) (Just node)

-- | Remove and return the element at the back of the deque.
pop :: Deque a -> IO (Maybe a)
pop deque = do
  mTail <- readIORef (dqTail deque)
  case mTail of
    Nothing        -> pure Nothing
    Just tailNode  -> do
      prevNode <- readIORef (nodePrev tailNode)
      writeIORef (dqTail deque) prevNode
      case prevNode of
        Nothing       -> writeIORef (dqHead deque) Nothing
        Just prevN    -> writeIORef (nodeNext prevN) Nothing
      pure (Just (nodeValue tailNode))

-- | Remove and return the element at the front of the deque.
shift :: Deque a -> IO (Maybe a)
shift deque = do
  mHead <- readIORef (dqHead deque)
  case mHead of
    Nothing        -> pure Nothing
    Just headNode  -> do
      nextNode <- readIORef (nodeNext headNode)
      writeIORef (dqHead deque) nextNode
      case nextNode of
        Nothing       -> writeIORef (dqTail deque) Nothing
        Just nextN    -> writeIORef (nodePrev nextN) Nothing
      pure (Just (nodeValue headNode))
