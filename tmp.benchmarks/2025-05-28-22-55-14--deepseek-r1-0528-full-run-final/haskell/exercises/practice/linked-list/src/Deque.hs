module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Node
    { value :: a
    , prevRef :: IORef (Maybe (Node a))
    , nextRef :: IORef (Maybe (Node a))
    }

data Deque a = Deque
    { headRef :: IORef (Maybe (Node a))
    , tailRef :: IORef (Maybe (Node a))
    }

mkDeque :: IO (Deque a)
mkDeque = do
    h <- newIORef Nothing
    t <- newIORef Nothing
    return $ Deque h t

push :: Deque a -> a -> IO ()
push deque x = do
    mt <- readIORef (tailRef deque)
    case mt of
        Nothing -> do
            newPrev <- newIORef Nothing
            newNext <- newIORef Nothing
            let newNode = Node x newPrev newNext
            writeIORef (headRef deque) (Just newNode)
            writeIORef (tailRef deque) (Just newNode)
        Just tNode -> do
            newPrev <- newIORef (Just tNode)
            newNext <- newIORef Nothing
            let newNode = Node x newPrev newNext
            writeIORef (nextRef tNode) (Just newNode)
            writeIORef (tailRef deque) (Just newNode)

unshift :: Deque a -> a -> IO ()
unshift deque x = do
    mh <- readIORef (headRef deque)
    case mh of
        Nothing -> do
            newPrev <- newIORef Nothing
            newNext <- newIORef Nothing
            let newNode = Node x newPrev newNext
            writeIORef (headRef deque) (Just newNode)
            writeIORef (tailRef deque) (Just newNode)
        Just hNode -> do
            newPrev <- newIORef Nothing
            newNext <- newIORef (Just hNode)
            let newNode = Node x newPrev newNext
            writeIORef (prevRef hNode) (Just newNode)
            writeIORef (headRef deque) (Just newNode)

pop :: Deque a -> IO (Maybe a)
pop deque = do
    mt <- readIORef (tailRef deque)
    case mt of
        Nothing -> return Nothing
        Just tNode -> do
            mprev <- readIORef (prevRef tNode)
            case mprev of
                Nothing -> do
                    writeIORef (headRef deque) Nothing
                    writeIORef (tailRef deque) Nothing
                Just prevNode -> do
                    writeIORef (nextRef prevNode) Nothing
                    writeIORef (tailRef deque) (Just prevNode)
            return (Just (value tNode))

shift :: Deque a -> IO (Maybe a)
shift deque = do
    mh <- readIORef (headRef deque)
    case mh of
        Nothing -> return Nothing
        Just hNode -> do
            mnext <- readIORef (nextRef hNode)
            case mnext of
                Nothing -> do
                    writeIORef (headRef deque) Nothing
                    writeIORef (tailRef deque) Nothing
                Just nextNode -> do
                    writeIORef (prevRef nextNode) Nothing
                    writeIORef (headRef deque) (Just nextNode)
            return (Just (value hNode))
