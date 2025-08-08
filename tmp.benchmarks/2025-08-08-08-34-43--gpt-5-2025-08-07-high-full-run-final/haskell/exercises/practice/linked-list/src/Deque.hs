module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Internal node for the doubly linked list
data Node a = Node
  { nPrev  :: IORef (Maybe (Node a))
  , nValue :: a
  , nNext  :: IORef (Maybe (Node a))
  }

-- The Deque keeps IORefs to the front (head) and back (tail) nodes
data Deque a = Deque
  { dFront :: IORef (Maybe (Node a))
  , dBack  :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  b <- newIORef Nothing
  pure (Deque f b)

-- Helper: construct a new node with given neighbors
newNode :: Maybe (Node a) -> a -> Maybe (Node a) -> IO (Node a)
newNode mPrev v mNext = do
  p <- newIORef mPrev
  n <- newIORef mNext
  pure (Node p v n)

push :: Deque a -> a -> IO ()
push dq x = do
  mBack <- readIORef (dBack dq)
  case mBack of
    Nothing -> do
      node <- newNode Nothing x Nothing
      writeIORef (dFront dq) (Just node)
      writeIORef (dBack dq) (Just node)
    Just tailNode -> do
      node <- newNode (Just tailNode) x Nothing
      writeIORef (nNext tailNode) (Just node)
      writeIORef (dBack dq) (Just node)

unshift :: Deque a -> a -> IO ()
unshift dq x = do
  mFront <- readIORef (dFront dq)
  case mFront of
    Nothing -> do
      node <- newNode Nothing x Nothing
      writeIORef (dFront dq) (Just node)
      writeIORef (dBack dq) (Just node)
    Just headNode -> do
      node <- newNode Nothing x (Just headNode)
      writeIORef (nPrev headNode) (Just node)
      writeIORef (dFront dq) (Just node)

pop :: Deque a -> IO (Maybe a)
pop dq = do
  mBack <- readIORef (dBack dq)
  case mBack of
    Nothing -> pure Nothing
    Just tailNode -> do
      let v = nValue tailNode
      mPrev <- readIORef (nPrev tailNode)
      case mPrev of
        Nothing -> do
          -- Only one element
          writeIORef (dFront dq) Nothing
          writeIORef (dBack dq) Nothing
        Just prevNode -> do
          writeIORef (nNext prevNode) Nothing
          writeIORef (dBack dq) (Just prevNode)
      -- Help GC by breaking popped node's links
      writeIORef (nPrev tailNode) Nothing
      writeIORef (nNext tailNode) Nothing
      pure (Just v)

shift :: Deque a -> IO (Maybe a)
shift dq = do
  mFront <- readIORef (dFront dq)
  case mFront of
    Nothing -> pure Nothing
    Just headNode -> do
      let v = nValue headNode
      mNext <- readIORef (nNext headNode)
      case mNext of
        Nothing -> do
          -- Only one element
          writeIORef (dFront dq) Nothing
          writeIORef (dBack dq) Nothing
        Just nextNode -> do
          writeIORef (nPrev nextNode) Nothing
          writeIORef (dFront dq) (Just nextNode)
      -- Help GC by breaking shifted node's links
      writeIORef (nPrev headNode) Nothing
      writeIORef (nNext headNode) Nothing
      pure (Just v)
