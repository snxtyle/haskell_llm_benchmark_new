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

-- A singly linked list is either empty (Nil) or a node (Cons)
-- containing a datum and a pointer to the next list.
data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

-- Creates an empty list.
nil :: LinkedList a
nil = Nil

-- Creates a new list node with a given datum and a pointer to the next list.
new :: a -> LinkedList a -> LinkedList a
new x linkedList = Cons x linkedList

-- Checks if a list is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Gets the datum from the head of a non-empty list.
-- This is a partial function; calling it on an empty list will cause an error.
datum :: LinkedList a -> a
datum (Cons x _) = x
datum Nil        = error "datum is not defined for an empty list"

-- Gets the next list (tail) from a non-empty list.
-- This is a partial function; calling it on an empty list will cause an error.
next :: LinkedList a -> LinkedList a
next (Cons _ xs) = xs
next Nil         = error "next is not defined for an empty list"

-- Converts a standard Haskell list to a LinkedList.
fromList :: [a] -> LinkedList a
fromList = foldr new nil

-- Converts a LinkedList to a standard Haskell list.
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs

-- Reverses a LinkedList.
-- This is implemented efficiently using a helper function with an accumulator.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = reverseHelper linkedList nil
  where
    reverseHelper :: LinkedList a -> LinkedList a -> LinkedList a
    reverseHelper Nil acc = acc
    reverseHelper (Cons x xs) acc = reverseHelper xs (new x acc)
