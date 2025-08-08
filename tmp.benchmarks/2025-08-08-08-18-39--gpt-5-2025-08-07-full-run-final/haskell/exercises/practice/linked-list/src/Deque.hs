module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { nPrev :: IORef (Maybe (Node a))
  , nVal  :: a
  , nNext :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { dFront :: IORef (Maybe (Node a))
  , dBack  :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  b <- newIORef Nothing
  pure (Deque f b)

pop :: Deque a -> IO (Maybe a)
pop dq = do
  mBack <- readIORef (dBack dq)
  case mBack of
    Nothing -> pure Nothing
    Just backNode -> do
      let v = nVal backNode
      mPrev <- readIORef (nPrev backNode)
      case mPrev of
        Nothing -> do
          -- Only one element; deque becomes empty
          writeIORef (dFront dq) Nothing
          writeIORef (dBack dq) Nothing
        Just prevNode -> do
          -- Detach last node
          writeIORef (nNext prevNode) Nothing
          writeIORef (dBack dq) (Just prevNode)
      -- Clear removed node links (tidy up)
      writeIORef (nPrev backNode) Nothing
      writeIORef (nNext backNode) Nothing
      pure (Just v)

push :: Deque a -> a -> IO ()
push dq x = do
  node <- newNode x
  mBack <- readIORef (dBack dq)
  case mBack of
    Nothing -> do
      -- Empty deque; new node is both front and back
      writeIORef (dFront dq) (Just node)
      writeIORef (dBack dq) (Just node)
    Just backNode -> do
      writeIORef (nPrev node) (Just backNode)
      writeIORef (nNext backNode) (Just node)
      writeIORef (dBack dq) (Just node)

unshift :: Deque a -> a -> IO ()
unshift dq x = do
  node <- newNode x
  mFront <- readIORef (dFront dq)
  case mFront of
    Nothing -> do
      -- Empty deque; new node is both front and back
      writeIORef (dFront dq) (Just node)
      writeIORef (dBack dq) (Just node)
    Just frontNode -> do
      writeIORef (nNext node) (Just frontNode)
      writeIORef (nPrev frontNode) (Just node)
      writeIORef (dFront dq) (Just node)

shift :: Deque a -> IO (Maybe a)
shift dq = do
  mFront <- readIORef (dFront dq)
  case mFront of
    Nothing -> pure Nothing
    Just frontNode -> do
      let v = nVal frontNode
      mNext <- readIORef (nNext frontNode)
      case mNext of
        Nothing -> do
          -- Only one element; deque becomes empty
          writeIORef (dFront dq) Nothing
          writeIORef (dBack dq) Nothing
        Just nextNode -> do
          -- Detach first node
          writeIORef (nPrev nextNode) Nothing
          writeIORef (dFront dq) (Just nextNode)
      -- Clear removed node links (tidy up)
      writeIORef (nPrev frontNode) Nothing
      writeIORef (nNext frontNode) Nothing
      pure (Just v)

newNode :: a -> IO (Node a)
newNode x = do
  p <- newIORef Nothing
  n <- newIORef Nothing
  pure (Node p x n)
