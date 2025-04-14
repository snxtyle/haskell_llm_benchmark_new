module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Doubly linked list node
data Node a = Node
  { prev :: IORef (Maybe (Node a))
  , value :: a
  , next :: IORef (Maybe (Node a))
  }

-- Deque with references to head and tail nodes, and size
data Deque a = Deque
  { headRef :: IORef (Maybe (Node a))
  , tailRef :: IORef (Maybe (Node a))
  , sizeRef :: IORef Int
  }

mkDeque :: IO (Deque a)
mkDeque = do
  h <- newIORef Nothing
  t <- newIORef Nothing
  s <- newIORef 0
  return $ Deque h t s

push :: Deque a -> a -> IO ()
push dq x = do
  t <- readIORef (tailRef dq)
  nodePrevRef <- newIORef t
  nodeNextRef <- newIORef Nothing
  let node = Node nodePrevRef x nodeNextRef
  case t of
    Nothing -> do
      writeIORef (headRef dq) (Just node)
      writeIORef (tailRef dq) (Just node)
    Just oldTail -> do
      writeIORef (next oldTail) (Just node)
      writeIORef (tailRef dq) (Just node)
  modifyIORef' (sizeRef dq) (+1)

unshift :: Deque a -> a -> IO ()
unshift dq x = do
  h <- readIORef (headRef dq)
  nodePrevRef <- newIORef Nothing
  nodeNextRef <- newIORef h
  let node = Node nodePrevRef x nodeNextRef
  case h of
    Nothing -> do
      writeIORef (headRef dq) (Just node)
      writeIORef (tailRef dq) (Just node)
    Just oldHead -> do
      writeIORef (prev oldHead) (Just node)
      writeIORef (headRef dq) (Just node)
  modifyIORef' (sizeRef dq) (+1)

pop :: Deque a -> IO (Maybe a)
pop dq = do
  t <- readIORef (tailRef dq)
  case t of
    Nothing -> return Nothing
    Just node -> do
      prevNode <- readIORef (prev node)
      case prevNode of
        Nothing -> do
          writeIORef (headRef dq) Nothing
          writeIORef (tailRef dq) Nothing
        Just pn -> do
          writeIORef (next pn) Nothing
          writeIORef (tailRef dq) (Just pn)
      modifyIORef' (sizeRef dq) (subtract 1)
      return (Just (value node))

shift :: Deque a -> IO (Maybe a)
shift dq = do
  h <- readIORef (headRef dq)
  case h of
    Nothing -> return Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          writeIORef (headRef dq) Nothing
          writeIORef (tailRef dq) Nothing
        Just nn -> do
          writeIORef (prev nn) Nothing
          writeIORef (headRef dq) (Just nn)
      modifyIORef' (sizeRef dq) (subtract 1)
      return (Just (value node))
