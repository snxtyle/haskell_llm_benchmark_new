module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Internal node type for the doubly linked list
data Node a = Node
  { val  :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }

-- The Deque holds references to the front and back nodes
data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back  :: IORef (Maybe (Node a))
  }

-- Create a new empty deque
mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  b <- newIORef Nothing
  return $ Deque f b

-- Push to the back of the deque
push :: Deque a -> a -> IO ()
push dq x = do
  mbBack <- readIORef (back dq)
  prevRef <- newIORef mbBack
  nextRef <- newIORef Nothing
  let node = Node x prevRef nextRef
  writeIORef (back dq) (Just node)
  case mbBack of
    Nothing      -> writeIORef (front dq) (Just node)
    Just oldBack -> writeIORef (next oldBack) (Just node)

-- Pop from the back of the deque
pop :: Deque a -> IO (Maybe a)
pop dq = do
  mbBack <- readIORef (back dq)
  case mbBack of
    Nothing   -> return Nothing
    Just node -> do
      mbPrev <- readIORef (prev node)
      writeIORef (back dq) mbPrev
      case mbPrev of
        Nothing      -> writeIORef (front dq) Nothing
        Just prevNd  -> writeIORef (next prevNd) Nothing
      return (Just (val node))

-- Unshift (push to the front) of the deque
unshift :: Deque a -> a -> IO ()
unshift dq x = do
  mbFront <- readIORef (front dq)
  prevRef <- newIORef Nothing
  nextRef <- newIORef mbFront
  let node = Node x prevRef nextRef
  writeIORef (front dq) (Just node)
  case mbFront of
    Nothing       -> writeIORef (back dq) (Just node)
    Just oldFront -> writeIORef (prev oldFront) (Just node)

-- Shift (pop from the front) of the deque
shift :: Deque a -> IO (Maybe a)
shift dq = do
  mbFront <- readIORef (front dq)
  case mbFront of
    Nothing   -> return Nothing
    Just node -> do
      mbNext <- readIORef (next node)
      writeIORef (front dq) mbNext
      case mbNext of
        Nothing       -> writeIORef (back dq) Nothing
        Just nextNd   -> writeIORef (prev nextNd) Nothing
      return (Just (val node))
