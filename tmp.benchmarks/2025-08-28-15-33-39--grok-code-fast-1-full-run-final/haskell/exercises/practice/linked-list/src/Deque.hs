module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

type NodeRef a = IORef (Node a)

data Node a = Node { val :: a, prev :: Maybe (NodeRef a), next :: Maybe (NodeRef a) }

newtype Deque a = Deque (IORef (Maybe (NodeRef a), Maybe (NodeRef a)))

mkDeque :: IO (Deque a)
mkDeque = do
  ht <- newIORef (Nothing, Nothing)
  return (Deque ht)

pop :: Deque a -> IO (Maybe a)
pop (Deque htRef) = do
  (mh, mt) <- readIORef htRef
  case mt of
    Nothing -> return Nothing
    Just tailRef -> do
      tailNode <- readIORef tailRef
      let v = val tailNode
      case prev tailNode of
        Nothing -> writeIORef htRef (Nothing, Nothing)
        Just newTailRef -> do
          newTailNode <- readIORef newTailRef
          let updatedNewTail = newTailNode { next = Nothing }
          writeIORef newTailRef updatedNewTail
          writeIORef htRef (mh, Just newTailRef)
      return (Just v)

push :: Deque a -> a -> IO ()
push (Deque htRef) x = do
  (mh, mt) <- readIORef htRef
  let newNode = Node x mt Nothing
  newNodeRef <- newIORef newNode
  case mt of
    Nothing -> writeIORef htRef (Just newNodeRef, Just newNodeRef)
    Just tailRef -> do
      tailNode <- readIORef tailRef
      let updatedTail = tailNode { next = Just newNodeRef }
      writeIORef tailRef updatedTail
      writeIORef htRef (mh, Just newNodeRef)

unshift :: Deque a -> a -> IO ()
unshift (Deque htRef) x = do
  (mh, mt) <- readIORef htRef
  let newNode = Node x Nothing mh
  newNodeRef <- newIORef newNode
  case mh of
    Nothing -> writeIORef htRef (Just newNodeRef, Just newNodeRef)
    Just headRef -> do
      headNode <- readIORef headRef
      let updatedHead = headNode { prev = Just newNodeRef }
      writeIORef headRef updatedHead
      writeIORef htRef (Just newNodeRef, mt)

shift :: Deque a -> IO (Maybe a)
shift (Deque htRef) = do
  (mh, mt) <- readIORef htRef
  case mh of
    Nothing -> return Nothing
    Just headRef -> do
      headNode <- readIORef headRef
      let v = val headNode
      case next headNode of
        Nothing -> writeIORef htRef (Nothing, Nothing)
        Just newHeadRef -> do
          newHeadNode <- readIORef newHeadRef
          let updatedNewHead = newHeadNode { prev = Nothing }
          writeIORef newHeadRef updatedNewHead
          writeIORef htRef (Just newHeadRef, mt)
      return (Just v)
