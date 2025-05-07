module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Prelude hiding (head, tail)

-- | Internal node of the doubly linked list
data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

-- | The deque keeps references to the first and last node.
data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back  :: IORef (Maybe (Node a))
  }

-- | Create an empty deque.
mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  b <- newIORef Nothing
  pure $ Deque f b

-- | Push a value onto the *back* of the deque.
push :: Deque a -> a -> IO ()
push dq x = do
  node <- newNode x
  mbBack <- readIORef (back dq)
  case mbBack of
    Nothing -> do
      -- Deque is empty: new node becomes both front and back.
      writeIORef (front dq) (Just node)
      writeIORef (back  dq) (Just node)
    Just oldBack -> do
      -- Link the new node after the current back.
      writeIORef (prev node) (Just oldBack)
      writeIORef (next oldBack) (Just node)
      writeIORef (back dq) (Just node)

-- | Add a value to the *front* of the deque.
unshift :: Deque a -> a -> IO ()
unshift dq x = do
  node <- newNode x
  mbFront <- readIORef (front dq)
  case mbFront of
    Nothing -> do
      -- Deque is empty: new node becomes both front and back.
      writeIORef (front dq) (Just node)
      writeIORef (back  dq) (Just node)
    Just oldFront -> do
      -- Link the new node in front of the current front.
      writeIORef (next node) (Just oldFront)
      writeIORef (prev oldFront) (Just node)
      writeIORef (front dq) (Just node)

-- | Pop a value from the *back* of the deque.
pop :: Deque a -> IO (Maybe a)
pop dq = do
  mbBack <- readIORef (back dq)
  case mbBack of
    Nothing -> pure Nothing
    Just node -> do
      prevNode <- readIORef (prev node)
      case prevNode of
        Nothing -> do
          -- Removing the last element leaves the deque empty.
          writeIORef (front dq) Nothing
          writeIORef (back  dq) Nothing
        Just p -> do
          writeIORef (next p) Nothing
          writeIORef (back dq) (Just p)
      pure . Just $ value node

-- | Remove and return a value from the *front* of the deque.
shift :: Deque a -> IO (Maybe a)
shift dq = do
  mbFront <- readIORef (front dq)
  case mbFront of
    Nothing -> pure Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          -- Removing the last element leaves the deque empty.
          writeIORef (front dq) Nothing
          writeIORef (back  dq) Nothing
        Just n -> do
          writeIORef (prev n) Nothing
          writeIORef (front dq) (Just n)
      pure . Just $ value node

-- | Helper to create a standalone node.
newNode :: a -> IO (Node a)
newNode x = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  pure $ Node x p n
