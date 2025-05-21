module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef
import Control.Monad (when)

-- A Node in the doubly linked list
data Node a = Node
    { nodeValue :: a
    , prevNode  :: IORef (Maybe (Node a))
    , nextNode  :: IORef (Maybe (Node a))
    }

-- The Deque itself, holding references to the head, tail, and size
data Deque a = Deque
    { headRef :: IORef (Maybe (Node a))
    , tailRef :: IORef (Maybe (Node a))
    , sizeRef :: IORef Int
    }

-- Helper to create a new node with empty prev/next references
newNodeRef :: a -> IO (Node a)
newNodeRef val = do
    pRef <- newIORef Nothing
    nRef <- newIORef Nothing
    return $ Node val pRef nRef

mkDeque :: IO (Deque a)
mkDeque = do
    hRef <- newIORef Nothing
    tRef <- newIORef Nothing
    sRef <- newIORef 0
    return $ Deque hRef tRef sRef

push :: Deque a -> a -> IO ()
push (Deque hRef tRef sRef) x = do
    newNode <- newNodeRef x
    currentTail <- readIORef tRef
    case currentTail of
        Nothing -> do -- Deque is empty
            writeIORef hRef (Just newNode)
            writeIORef tRef (Just newNode)
        Just oldTail -> do
            writeIORef (nextNode oldTail) (Just newNode)
            writeIORef (prevNode newNode) (Just oldTail)
            writeIORef tRef (Just newNode)
    modifyIORef' sRef (+ 1)

unshift :: Deque a -> a -> IO ()
unshift (Deque hRef tRef sRef) x = do
    newNode <- newNodeRef x
    currentHead <- readIORef hRef
    case currentHead of
        Nothing -> do -- Deque is empty
            writeIORef hRef (Just newNode)
            writeIORef tRef (Just newNode)
        Just oldHead -> do
            writeIORef (prevNode oldHead) (Just newNode)
            writeIORef (nextNode newNode) (Just oldHead)
            writeIORef hRef (Just newNode)
    modifyIORef' sRef (+ 1)

pop :: Deque a -> IO (Maybe a)
pop (Deque hRef tRef sRef) = do
    currentTail <- readIORef tRef
    case currentTail of
        Nothing -> return Nothing -- Deque is empty
        Just oldTail -> do
            modifyIORef' sRef (subtract 1)
            newTail <- readIORef (prevNode oldTail)
            case newTail of
                Nothing -> do -- oldTail was the only element
                    writeIORef hRef Nothing
                    writeIORef tRef Nothing
                Just nt -> do
                    writeIORef (nextNode nt) Nothing -- Disconnect oldTail from newTail
                    writeIORef tRef (Just nt)
            return (Just (nodeValue oldTail))

shift :: Deque a -> IO (Maybe a)
shift (Deque hRef tRef sRef) = do
    currentHead <- readIORef hRef
    case currentHead of
        Nothing -> return Nothing -- Deque is empty
        Just oldHead -> do
            modifyIORef' sRef (subtract 1)
            newHead <- readIORef (nextNode oldHead)
            case newHead of
                Nothing -> do -- oldHead was the only element
                    writeIORef hRef Nothing
                    writeIORef tRef Nothing
                Just nh -> do
                    writeIORef (prevNode nh) Nothing -- Disconnect oldHead from newHead
                    writeIORef hRef (Just nh)
            return (Just (nodeValue oldHead))
