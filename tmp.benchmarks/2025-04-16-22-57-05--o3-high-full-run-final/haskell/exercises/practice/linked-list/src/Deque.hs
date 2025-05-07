module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Prelude

-- | Internal node of the doubly‑linked list.
data Node a = Node
  { nodeValue :: a
  , nodePrev  :: IORef (Maybe (Node a))
  , nodeNext  :: IORef (Maybe (Node a))
  }

-- | A deque keeps mutable references to the head and tail nodes.
data Deque a = Deque
  { dequeHead :: IORef (Maybe (Node a))
  , dequeTail :: IORef (Maybe (Node a))
  }

-- | Create an empty deque.
mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  pure (Deque h t)

-- | Push an element onto the right end (tail) of the deque.
push :: Deque a -> a -> IO ()
push dq x = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  mTail <- readIORef (dequeTail dq)
  case mTail of
    Nothing -> do
      writeIORef (dequeHead dq) (Just newNode)
      writeIORef (dequeTail dq) (Just newNode)
    Just oldTail -> do
      writeIORef (nodeNext oldTail) (Just newNode)
      writeIORef prevRef (Just oldTail)
      writeIORef (dequeTail dq) (Just newNode)

-- | Insert an element at the left end (head) of the deque.
unshift :: Deque a -> a -> IO ()
unshift dq x = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  mHead <- readIORef (dequeHead dq)
  case mHead of
    Nothing -> do
      writeIORef (dequeHead dq) (Just newNode)
      writeIORef (dequeTail dq) (Just newNode)
    Just oldHead -> do
      writeIORef (nodePrev oldHead) (Just newNode)
      writeIORef nextRef (Just oldHead)
      writeIORef (dequeHead dq) (Just newNode)

-- | Remove and return the element at the right end (tail) of the deque.
pop :: Deque a -> IO (Maybe a)
pop dq = do
  mTail <- readIORef (dequeTail dq)
  case mTail of
    Nothing       -> pure Nothing
    Just tailNode -> do
      mPrev <- readIORef (nodePrev tailNode)
      case mPrev of
        Nothing        -> do
          writeIORef (dequeHead dq) Nothing
          writeIORef (dequeTail dq) Nothing
        Just prevNode  -> do
          writeIORef (nodeNext prevNode) Nothing
          writeIORef (dequeTail dq) (Just prevNode)
      pure (Just (nodeValue tailNode))

-- | Remove and return the element at the left end (head) of the deque.
shift :: Deque a -> IO (Maybe a)
shift dq = do
  mHead <- readIORef (dequeHead dq)
  case mHead of
    Nothing       -> pure Nothing
    Just headNode -> do
      mNext <- readIORef (nodeNext headNode)
      case mNext of
        Nothing        -> do
          writeIORef (dequeHead dq) Nothing
          writeIORef (dequeTail dq) Nothing
        Just nextNode  -> do
          writeIORef (nodePrev nextNode) Nothing
          writeIORef (dequeHead dq) (Just nextNode)
      pure (Just (nodeValue headNode))
