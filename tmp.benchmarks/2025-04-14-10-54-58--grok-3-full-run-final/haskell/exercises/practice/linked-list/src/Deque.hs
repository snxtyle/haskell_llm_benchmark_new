module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Node in the doubly-linked list
data Node a = Node {
    value :: a,
    prev :: IORef (Maybe (Node a)),
    next :: IORef (Maybe (Node a))
}

-- Deque structure holding references to first and last nodes
data Deque a = Deque {
    front :: IORef (Maybe (Node a)),
    back :: IORef (Maybe (Node a))
}

-- Create an empty Deque
mkDeque :: IO (Deque a)
mkDeque = do
    frontRef <- newIORef Nothing
    backRef <- newIORef Nothing
    return $ Deque frontRef backRef

-- Remove and return the last element
pop :: Deque a -> IO (Maybe a)
pop deque = do
    backNode <- readIORef (back deque)
    case backNode of
        Nothing -> return Nothing
        Just node -> do
            let val = value node
            prevNode <- readIORef (prev node)
            writeIORef (back deque) prevNode
            case prevNode of
                Just p -> writeIORef (next p) Nothing
                Nothing -> writeIORef (front deque) Nothing
            return $ Just val

-- Add an element to the end
push :: Deque a -> a -> IO ()
push deque x = do
    newNodePrev <- readIORef (back deque)
    newNext <- newIORef Nothing
    newPrev <- newIORef newNodePrev
    let newNode = Node x newPrev newNext
    case newNodePrev of
        Just oldBack -> writeIORef (next oldBack) (Just newNode)
        Nothing -> writeIORef (front deque) (Just newNode)
    writeIORef (back deque) (Just newNode)

-- Add an element to the beginning
unshift :: Deque a -> a -> IO ()
unshift deque x = do
    newNodeNext <- readIORef (front deque)
    newPrev <- newIORef Nothing
    newNext <- newIORef newNodeNext
    let newNode = Node x newPrev newNext
    case newNodeNext of
        Just oldFront -> writeIORef (prev oldFront) (Just newNode)
        Nothing -> writeIORef (back deque) (Just newNode)
    writeIORef (front deque) (Just newNode)

-- Remove and return the first element
shift :: Deque a -> IO (Maybe a)
shift deque = do
    frontNode <- readIORef (front deque)
    case frontNode of
        Nothing -> return Nothing
        Just node -> do
            let val = value node
            nextNode <- readIORef (next node)
            writeIORef (front deque) nextNode
            case nextNode of
                Just n -> writeIORef (prev n) Nothing
                Nothing -> writeIORef (back deque) Nothing
            return $ Just val
