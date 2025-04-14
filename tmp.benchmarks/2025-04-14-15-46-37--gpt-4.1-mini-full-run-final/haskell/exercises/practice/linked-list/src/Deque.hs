module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Control.Monad (when)
import Prelude hiding (last)

data Node a = Node
  { value :: a
  , prev  :: IORef (Maybe (Node a))
  , next  :: IORef (Maybe (Node a))
  }

data Deque a = Deque
  { first :: IORef (Maybe (Node a))
  , last  :: IORef (Maybe (Node a))
  }

mkDeque :: IO (Deque a)
mkDeque = do
  f <- newIORef Nothing
  l <- newIORef Nothing
  return $ Deque f l

pop :: Deque a -> IO (Maybe a)
pop deque = do
  mlastNode <- readIORef (last deque)
  case mlastNode of
    Nothing -> return Nothing
    Just node -> do
      mprevNode <- readIORef (prev node)
      case mprevNode of
        Nothing -> do
          -- This was the only node
          writeIORef (first deque) Nothing
          writeIORef (last deque) Nothing
        Just pnode -> do
          writeIORef (next pnode) Nothing
          writeIORef (last deque) (Just pnode)
      return $ Just (value node)

push :: Deque a -> a -> IO ()
push deque x = do
  mlastNode <- readIORef (last deque)
  prevRef <- newIORef mlastNode
  nextRef <- newIORef Nothing
  let newNode = Node x prevRef nextRef
  case mlastNode of
    Nothing -> do
      -- empty deque
      writeIORef (first deque) (Just newNode)
      writeIORef (last deque) (Just newNode)
    Just lastNode -> do
      writeIORef (next lastNode) (Just newNode)
      writeIORef (last deque) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
  mfirstNode <- readIORef (first deque)
  prevRef <- newIORef Nothing
  nextRef <- newIORef mfirstNode
  let newNode = Node x prevRef nextRef
  case mfirstNode of
    Nothing -> do
      -- empty deque
      writeIORef (first deque) (Just newNode)
      writeIORef (last deque) (Just newNode)
    Just firstNode -> do
      writeIORef (prev firstNode) (Just newNode)
      writeIORef (first deque) (Just newNode)

shift :: Deque a -> IO (Maybe a)
shift deque = do
  mfirstNode <- readIORef (first deque)
  case mfirstNode of
    Nothing -> return Nothing
    Just node -> do
      mnextNode <- readIORef (next node)
      case mnextNode of
        Nothing -> do
          -- only one node
          writeIORef (first deque) Nothing
          writeIORef (last deque) Nothing
        Just nnode -> do
          writeIORef (prev nnode) Nothing
          writeIORef (first deque) (Just nnode)
      return $ Just (value node)
