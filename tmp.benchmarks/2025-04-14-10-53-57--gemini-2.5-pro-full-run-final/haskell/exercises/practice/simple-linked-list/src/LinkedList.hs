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

-- Define the LinkedList data type.
-- It can be either 'Nil' (representing an empty list)
-- or a 'Node' containing a value 'a' and the rest of the list 'LinkedList a'.
data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

-- Returns the value of the head node.
-- Raises an error if the list is empty.
datum :: LinkedList a -> a
datum Nil = error "datum: list is empty"
datum (Node x _) = x

-- Converts a standard Haskell list to a LinkedList.
-- Uses foldr to build the list from right to left.
fromList :: [a] -> LinkedList a
fromList = foldr new nil
-- Alternative recursive implementation:
-- fromList [] = Nil
-- fromList (x:xs) = Node x (fromList xs)

-- Checks if the LinkedList is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Creates a new LinkedList node.
-- Prepends the value 'a' to the given LinkedList.
new :: a -> LinkedList a -> LinkedList a
new = Node -- The Node constructor itself works as this function

-- Returns the tail of the LinkedList (everything after the head node).
-- Raises an error if the list is empty.
next :: LinkedList a -> LinkedList a
next Nil = error "next: list is empty"
next (Node _ xs) = xs

-- Represents an empty LinkedList.
nil :: LinkedList a
nil = Nil

-- Reverses a LinkedList.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = go list nil
  where
    -- Helper function using an accumulator 'acc' to build the reversed list.
    go :: LinkedList a -> LinkedList a -> LinkedList a
    go Nil acc = acc -- Base case: reached end of original list, return accumulated reversed list.
    go (Node x xs) acc = go xs (Node x acc) -- Recursive step: move head of current list to front of accumulator.

-- Converts a LinkedList back to a standard Haskell list.
toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
