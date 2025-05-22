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
          -- Only one element, deque becomes empty
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just prevN -> do
          -- Update the previous node to be the new back
          writeIORef (next prevN) Nothing
          writeIORef (back deque) (Just prevN)
      return $ Just (value node)

push :: Deque a -> a -> IO ()
push deque x = do
  newNode <- do
    prevRef <- newIORef Nothing
    nextRef <- newIORef Nothing
    return $ Node x prevRef nextRef
  
  backNode <- readIORef (back deque)
  case backNode of
    Nothing -> do
      -- Deque is empty
      writeIORef (front deque) (Just newNode)
      writeIORef (back deque) (Just newNode)
    Just oldBack -> do
      -- Link the new node to the old back
      writeIORef (prev newNode) (Just oldBack)
      writeIORef (next oldBack) (Just newNode)
      writeIORef (back deque) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  newNode <- do
    prevRef <- newIORef Nothing
    nextRef <- newIORef Nothing
    return $ Node x prevRef nextRef
  
  frontNode <- readIORef (front deque)
  case frontNode of
    Nothing -> do
      -- Deque is empty
      writeIORef (front deque) (Just newNode)
      writeIORef (back deque) (Just newNode)
    Just oldFront -> do
      -- Link the new node to the old front
      writeIORef (next newNode) (Just oldFront)
      writeIORef (prev oldFront) (Just newNode)
      writeIORef (front deque) (Just newNode)

shift :: Deque a -> IO (Maybe a)
shift deque = do
  frontNode <- readIORef (front deque)
  case frontNode of
    Nothing -> return Nothing
    Just node -> do
      nextNode <- readIORef (next node)
      case nextNode of
        Nothing -> do
          -- Only one element, deque becomes empty
          writeIORef (front deque) Nothing
          writeIORef (back deque) Nothing
        Just nextN -> do
          -- Update the next node to be the new front
          writeIORef (prev nextN) Nothing
          writeIORef (front deque) (Just nextN)
      return $ Just (value node)
