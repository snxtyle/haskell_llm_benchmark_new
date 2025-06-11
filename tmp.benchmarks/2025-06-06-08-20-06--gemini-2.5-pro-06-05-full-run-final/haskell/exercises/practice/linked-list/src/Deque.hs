module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
  { prev :: IORef (Maybe (Node a))
  , item :: a
  , next :: IORef (Maybe (Node a))
  }

data Deque a = Deque (IORef (Maybe (Node a))) (IORef (Maybe (Node a)))

mkDeque :: IO (Deque a)
mkDeque = do
  headRef <- newIORef Nothing
  tailRef <- newIORef Nothing
  return (Deque headRef tailRef)

push :: Deque a -> a -> IO ()
push (Deque headRef tailRef) x = do
  oldTail <- readIORef tailRef
  nextRef <- newIORef Nothing
  prevRef <- newIORef oldTail
  let newNode = Node {prev = prevRef, item = x, next = nextRef}

  case oldTail of
    Nothing -> writeIORef headRef (Just newNode)
    Just oldTailNode -> writeIORef (next oldTailNode) (Just newNode)

  writeIORef tailRef (Just newNode)

pop :: Deque a -> IO (Maybe a)
pop (Deque headRef tailRef) = do
  mTailNode <- readIORef tailRef
  case mTailNode of
    Nothing -> return Nothing
    Just tailNode -> do
      let val = item tailNode
      mPrevNode <- readIORef (prev tailNode)
      writeIORef tailRef mPrevNode

      case mPrevNode of
        Nothing -> writeIORef headRef Nothing
        Just prevNode -> writeIORef (next prevNode) Nothing

      return (Just val)

unshift :: Deque a -> a -> IO ()
unshift (Deque headRef tailRef) x = do
  oldHead <- readIORef headRef
  prevRef <- newIORef Nothing
  nextRef <- newIORef oldHead
  let newNode = Node {prev = prevRef, item = x, next = nextRef}

  case oldHead of
    Nothing -> writeIORef tailRef (Just newNode)
    Just oldHeadNode -> writeIORef (prev oldHeadNode) (Just newNode)

  writeIORef headRef (Just newNode)

shift :: Deque a -> IO (Maybe a)
shift (Deque headRef tailRef) = do
  mHeadNode <- readIORef headRef
  case mHeadNode of
    Nothing -> return Nothing
    Just headNode -> do
      let val = item headNode
      mNextNode <- readIORef (next headNode)
      writeIORef headRef mNextNode

      case mNextNode of
        Nothing -> writeIORef tailRef Nothing
        Just nextNode -> writeIORef (prev nextNode) Nothing

      return (Just val)
