module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

-- A LinkedList is either empty (Nil) or a Node containing a value 'a'
-- and the rest of the list (another LinkedList a).
data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

-- Returns the value at the head of the list.
-- Throws an error if the list is empty.
datum :: LinkedList a -> a
datum Nil = error "datum: list is empty"
datum (Node x _) = x

-- Converts a standard Haskell list to a LinkedList.
-- Example: fromList [1,2,3] == Node 1 (Node 2 (Node 3 Nil))
fromList :: [a] -> LinkedList a
fromList = foldr new Nil
-- Alternative recursive implementation:
-- fromList [] = Nil
-- fromList (x:xs) = Node x (fromList xs)

-- Checks if the LinkedList is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil (Node _ _) = False

-- Creates a new LinkedList node, prepending the value 'x'
-- to the existing 'linkedList'.
new :: a -> LinkedList a -> LinkedList a
new = Node
-- This is equivalent to:
-- new val rest = Node val rest

-- Returns the tail of the LinkedList.
-- Throws an error if the list is empty.
next :: LinkedList a -> LinkedList a
next Nil = error "next: list is empty"
next (Node _ rest) = rest

-- Represents an empty LinkedList.
nil :: LinkedList a
nil = Nil

-- Reverses a LinkedList.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = go Nil list
  where
    go :: LinkedList a -> LinkedList a -> LinkedList a
    go accumulator Nil = accumulator
    go accumulator (Node x xs) = go (Node x accumulator) xs

-- Converts a LinkedList to a standard Haskell list.
-- Example: toList (Node 1 (Node 2 (Node 3 Nil))) == [1,2,3]
toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
