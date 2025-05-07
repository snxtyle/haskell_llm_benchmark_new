module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

-- Node definition for the doubly linked list
-- Each node contains a value, and IORefs to the previous and next nodes.
data Node a = Node
    { prev :: IORef (Maybe (Node a))
    , value :: a
    , next :: IORef (Maybe (Node a))
    }

-- Deque definition
-- It holds IORefs to the head and tail nodes of the list.
data Deque a = Deque
    { head :: IORef (Maybe (Node a))
    , tail :: IORef (Maybe (Node a))
    }

-- Creates a new, empty Deque.
-- Both head and tail are initialized to Nothing.
mkDeque :: IO (Deque a)
mkDeque = do
    headRef <- newIORef Nothing
    tailRef <- newIORef Nothing
    return (Deque headRef tailRef)

-- Adds an element to the end (tail) of the Deque.
push :: Deque a -> a -> IO ()
push (Deque headRef tailRef) val = do
    mCurrentTailNode <- readIORef tailRef

    -- The new node's 'prev' will point to the current tail.
    -- Its 'next' will be Nothing as it's the new tail.
    newNodePrevRef <- newIORef mCurrentTailNode
    newNodeNextRef <- newIORef Nothing
    let newNode = Node { prev = newNodePrevRef, value = val, next = newNodeNextRef }

    case mCurrentTailNode of
        Nothing -> do -- Deque is empty, new node is both head and tail
            writeIORef headRef (Just newNode)
            writeIORef tailRef (Just newNode)
        Just currentTailNode -> do
            -- Link current tail's 'next' to the new node
            writeIORef (next currentTailNode) (Just newNode)
            -- The new node's 'prev' is already set to the current tail via newNodePrevRef.
            -- Update the deque's tail to be the new node
            writeIORef tailRef (Just newNode)

-- Removes and returns an element from the end (tail) of the Deque.
-- Returns Nothing if the Deque is empty.
pop :: Deque a -> IO (Maybe a)
pop (Deque headRef tailRef) = do
    mCurrentTailNode <- readIORef tailRef
    case mCurrentTailNode of
        Nothing -> return Nothing -- Deque is empty
        Just currentTailNode -> do
            let valToReturn = value currentTailNode
            -- The node that was previous to the current tail will become the new tail.
            mPrevNodeOfTail <- readIORef (prev currentTailNode)
            
            writeIORef tailRef mPrevNodeOfTail -- Update deque's tail
            
            case mPrevNodeOfTail of
                Nothing ->
                    -- This was the only element in the list.
                    -- So, the head also becomes Nothing.
                    writeIORef headRef Nothing
                Just prevNode ->
                    -- There are other elements. The new tail's 'next' must be Nothing.
                    writeIORef (next prevNode) Nothing
            
            return (Just valToReturn)

-- Adds an element to the beginning (head) of the Deque.
unshift :: Deque a -> a -> IO ()
unshift (Deque headRef tailRef) val = do
    mCurrentHeadNode <- readIORef headRef

    -- The new node's 'next' will point to the current head.
    -- Its 'prev' will be Nothing as it's the new head.
    newNodePrevRef <- newIORef Nothing
    newNodeNextRef <- newIORef mCurrentHeadNode
    let newNode = Node { prev = newNodePrevRef, value = val, next = newNodeNextRef }

    case mCurrentHeadNode of
        Nothing -> do -- Deque is empty, new node is both head and tail
            writeIORef headRef (Just newNode)
            writeIORef tailRef (Just newNode)
        Just currentHeadNode -> do
            -- Link current head's 'prev' to the new node
            writeIORef (prev currentHeadNode) (Just newNode)
            -- The new node's 'next' is already set to the current head via newNodeNextRef.
            -- Update the deque's head to be the new node
            writeIORef headRef (Just newNode)

-- Removes and returns an element from the beginning (head) of the Deque.
-- Returns Nothing if the Deque is empty.
shift :: Deque a -> IO (Maybe a)
shift (Deque headRef tailRef) = do
    mCurrentHeadNode <- readIORef headRef
    case mCurrentHeadNode of
        Nothing -> return Nothing -- Deque is empty
        Just currentHeadNode -> do
            let valToReturn = value currentHeadNode
            -- The node that was next to the current head will become the new head.
            mNextNodeOfHead <- readIORef (next currentHeadNode)

            writeIORef headRef mNextNodeOfHead -- Update deque's head

            case mNextNodeOfHead of
                Nothing ->
                    -- This was the only element in the list.
                    -- So, the tail also becomes Nothing.
                    writeIORef tailRef Nothing
                Just nextNode ->
                    -- There are other elements. The new head's 'prev' must be Nothing.
                    writeIORef (prev nextNode) Nothing
            
            return (Just valToReturn)
