module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node 
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

data Deque a = Deque 
  { head :: IORef (Maybe (Node a))
  , tail :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  headRef <- newIORef Nothing
  tailRef <- newIORef Nothing
  return $ Deque headRef tailRef

pop :: Deque a -> IO (Maybe a)
pop deque = do
  tailNode <- readIORef (tail deque)
  case tailNode of
    Nothing -> return Nothing
    Just node -> do
      val <- return (value node)
      prevNode <- readIORef (prev node)
      writeIORef (tail deque) prevNode
      case prevNode of
        Nothing -> writeIORef (head deque) Nothing
        Just p -> writeIORef (next p) Nothing
      return (Just val)

push :: Deque a -> a -> IO ()
push deque x = do
  newNode <- Node x <$> newIORef Nothing <*> newIORef Nothing
  tailNode <- readIORef (tail deque)
  writeIORef (prev newNode) tailNode
  writeIORef (tail deque) (Just newNode)
  case tailNode of
    Nothing -> writeIORef (head deque) (Just newNode)
    Just oldTail -> writeIORef (next oldTail) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  newNode <- Node x <$> newIORef Nothing <*> newIORef Nothing
  headNode <- readIORef (head deque)
  writeIORef (next newNode) headNode
  writeIORef (head deque) (Just newNode)
  case headNode of
    Nothing -> writeIORef (tail deque) (Just newNode)
    Just oldHead -> writeIORef (prev oldHead) (Just newNode)

shift :: Deque a -> IO (Maybe a)
shift deque = do
  headNode <- readIORef (head deque)
  case headNode of
    Nothing -> return Nothing
    Just node -> do
      val <- return (value node)
      nextNode <- readIORef (next node)
      writeIORef (head deque) nextNode
      case nextNode of
        Nothing -> writeIORef (tail deque) Nothing
        Just n -> writeIORef (prev n) Nothing
      return (Just val)
