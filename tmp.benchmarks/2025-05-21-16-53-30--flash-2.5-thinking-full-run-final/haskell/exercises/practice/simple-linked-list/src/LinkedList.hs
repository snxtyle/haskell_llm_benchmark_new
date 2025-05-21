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
-- Nil represents the end of the list (empty list).
-- Node a (LinkedList a) represents a node containing a value 'a' and a pointer to the next LinkedList.
data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

-- Returns the data (datum) of the current node.
-- Precondition: Should not be called on a Nil list.
datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil        = error "datum called on Nil LinkedList"

-- Converts a standard Haskell list to a LinkedList.
-- This builds the LinkedList by prepending elements, effectively creating it in the correct order.
fromList :: [a] -> LinkedList a
fromList []     = Nil
fromList (x:xs) = Node x (fromList xs)

-- Checks if the given LinkedList is empty (Nil).
isNil :: LinkedList a -> Bool
isNil Nil        = True
isNil (Node _ _) = False

-- Creates a new node with the given value and points to the provided LinkedList as its next.
new :: a -> LinkedList a -> LinkedList a
new x nextList = Node x nextList

-- Returns the rest of the list after the current node.
-- Precondition: Should not be called on a Nil list.
next :: LinkedList a -> LinkedList a
next (Node _ nextList) = nextList
next Nil               = error "next called on Nil LinkedList"

-- Represents an empty LinkedList.
nil :: LinkedList a
nil = Nil

-- Reverses the order of elements in a LinkedList.
-- This is done using an accumulator pattern for efficiency.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = rev list Nil
  where
    -- Helper function for reversal using an accumulator (acc)
    rev :: LinkedList a -> LinkedList a -> LinkedList a
    rev Nil acc            = acc -- When the original list is exhausted, return the accumulated reversed list.
    rev (Node x nextList) acc = rev nextList (Node x acc) -- Take the current node's value and prepend it to the accumulator.

-- Converts a LinkedList back to a standard Haskell list.
toList :: LinkedList a -> [a]
toList Nil            = []
toList (Node x nextList) = x : toList nextList
