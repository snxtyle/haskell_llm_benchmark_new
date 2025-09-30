module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { value :: a
  , prev :: IORef (Maybe (Node a))
  , next :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { front :: IORef (Maybe (Node a))
  , back :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  frontRef <- newIORef Nothing
  backRef <- newIORef Nothing
  return $ Deque frontRef backRef

pop :: Deque a -> IO (Maybe a)
pop deque = do
  backNode <- readIORef (back deque)
  case backNode of
    Nothing -> return Nothing
    Just node -> do
      prevNode <- readIORef (prev node)
      case prevNode of
        Nothing -> do
          -- Only one element
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just pNode -> do
          -- Multiple elements
          writeIORef (next pNode) Nothing
          writeIORef (back deque) (Just pNode)
      return $ Just (value node)

push :: Deque a -> a -> IO ()
push deque x = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  backNode <- readIORef (back deque)
  case backNode of
    Nothing -> do
      -- Empty deque
      let node = Node x prevRef nextRef
      writeIORef (front deque) (Just node)
      writeIORef (back deque) (Just node)
    Just bNode -> do
      -- Non-empty deque
      writeIORef prevRef (Just bNode)
      let node = Node x prevRef nextRef
      writeIORef (next bNode) (Just node)
      writeIORef (back deque) (Just node)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  prevRef <- newIORef Nothing
  nextRef <- newIORef Nothing
  frontNode <- readIORef (front deque)
  case frontNode of
    Nothing -> do
      -- Empty deque
      let node = Node x prevRef nextRef
      writeIORef (front deque) (Just node)
      writeIORef (back deque) (Just node)
    Just fNode -> do
      -- Non-empty deque
      writeIORef nextRef (Just fNode)
      let node = Node x prevRef nextRef
      writeIORef (prev fNode) (Just node)
      writeIORef (front deque) (Just node)

shift :: Deque a -> IO (Maybe a)
shift deque = do
  frontNode <- readIORef (front deque)
  case frontNode of
    Nothing -> return Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          -- Only one element
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just nNode -> do
          -- Multiple elements
          writeIORef (prev nNode) Nothing
          writeIORef (front deque) (Just nNode)
      return $ Just (value node)
