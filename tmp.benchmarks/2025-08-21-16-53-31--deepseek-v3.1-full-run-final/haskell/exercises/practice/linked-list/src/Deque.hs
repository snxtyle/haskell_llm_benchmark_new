module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node {
    value :: a,
    prev :: IORef (Maybe (Node a)),
    next :: IORef (Maybe (Node a))
}

data Deque a = Deque {
    headRef :: IORef (Maybe (Node a)),
    tailRef :: IORef (Maybe (Node a))
}

mkDeque :: IO (Deque a)
mkDeque = do
    head <- newIORef Nothing
    tail <- newIORef Nothing
    return $ Deque head tail

push :: Deque a -> a -> IO ()
push deque x = do
    newTail <- Node x <$> newIORef Nothing <*> newIORef Nothing
    tailNode <- readIORef (tailRef deque)
    case tailNode of
        Nothing -> do
            writeIORef (headRef deque) (Just newTail)
            writeIORef (tailRef deque) (Just newTail)
        Just oldTail -> do
            writeIORef (next oldTail) (Just newTail)
            writeIORef (prev newTail) (Just oldTail)
            writeIORef (tailRef deque) (Just newTail)

pop :: Deque a -> IO (Maybe a)
pop deque = do
    tailNode <- readIORef (tailRef deque)
    case tailNode of
        Nothing -> return Nothing
        Just node -> do
            prevNode <- readIORef (prev node)
            case prevNode of
                Nothing -> do
                    writeIORef (headRef deque) Nothing
                    writeIORef (tailRef deque) Nothing
                Just prevNode' -> do
                    writeIORef (next prevNode') Nothing
                    writeIORef (tailRef deque) (Just prevNode')
            return $ Just (value node)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    newHead <- Node x <$> newIORef Nothing <*> newIORef Nothing
    headNode <- readIORef (headRef deque)
    case headNode of
        Nothing -> do
            writeIORef (headRef deque) (Just newHead)
            writeIORef (tailRef deque) (Just newHead)
        Just oldHead -> do
            writeIORef (prev oldHead) (Just newHead)
            writeIORef (next newHead) (Just oldHead)
            writeIORef (headRef deque) (Just newHead)

shift :: Deque a -> IO (Maybe a)
shift deque = do
    headNode <- readIORef (headRef deque)
    case headNode of
        Nothing -> return Nothing
        Just node -> do
            nextNode <- readIORef (next node)
            case nextNode of
                Nothing -> do
                    writeIORef (headRef deque) Nothing
                    writeIORef (tailRef deque) Nothing
                Just nextNode' -> do
                    writeIORef (prev nextNode') Nothing
                    writeIORef (headRef deque) (Just nextNode')
            return $ Just (value node)
