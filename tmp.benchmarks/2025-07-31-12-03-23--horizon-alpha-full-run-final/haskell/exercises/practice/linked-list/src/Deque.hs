module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (void)

-- Internal node for the doubly linked list
data Node a = Node
  { nVal  :: a
  , nPrev :: IORef (Maybe (Node a))
  , nNext :: IORef (Maybe (Node a))
  }

-- Public Deque type holding references to head and tail
data Deque a = Deque
  { dHead :: IORef (Maybe (Node a))
  , dTail :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  pure (Deque h t)

-- Helper to create a new isolated node
newNode :: a -> IO (Node a)
newNode x = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  pure (Node x p n)

-- Remove the head node and return its value if present
shift :: Deque a -> IO (Maybe a)
shift (Deque hRef tRef) = do
  mHead <- readIORef hRef
  case mHead of
    Nothing -> pure Nothing
    Just headNode -> do
      mNext <- readIORef (nNext headNode)
      case mNext of
        Nothing -> do
          -- Only one element
          writeIORef hRef Nothing
          writeIORef tRef Nothing
        Just nextNode -> do
          -- More than one element, detach head
          writeIORef (nPrev nextNode) Nothing
          writeIORef hRef (Just nextNode)
      pure (Just (nVal headNode))

-- Remove the tail node and return its value if present
pop :: Deque a -> IO (Maybe a)
pop (Deque hRef tRef) = do
  mTail <- readIORef tRef
  case mTail of
    Nothing -> pure Nothing
    Just tailNode -> do
      mPrev <- readIORef (nPrev tailNode)
      case mPrev of
        Nothing -> do
          -- Only one element
          writeIORef hRef Nothing
          writeIORef tRef Nothing
        Just prevNode -> do
          -- More than one element, detach tail
          writeIORef (nNext prevNode) Nothing
          writeIORef tRef (Just prevNode)
      pure (Just (nVal tailNode))

-- Insert at the head
unshift :: Deque a -> a -> IO ()
unshift (Deque hRef tRef) x = do
  node <- newNode x
  mHead <- readIORef hRef
  case mHead of
    Nothing -> do
      -- Empty deque
      writeIORef hRef (Just node)
      writeIORef tRef (Just node)
    Just oldHead -> do
      writeIORef (nNext node) (Just oldHead)
      writeIORef (nPrev oldHead) (Just node)
      writeIORef hRef (Just node)

-- Insert at the tail
push :: Deque a -> a -> IO ()
push (Deque hRef tRef) x = do
  node <- newNode x
  mTail <- readIORef tRef
  case mTail of
    Nothing -> do
      -- Empty deque
      writeIORef hRef (Just node)
      writeIORef tRef (Just node)
    Just oldTail -> do
      writeIORef (nPrev node) (Just oldTail)
      writeIORef (nNext oldTail) (Just node)
      writeIORef tRef (Just node)
