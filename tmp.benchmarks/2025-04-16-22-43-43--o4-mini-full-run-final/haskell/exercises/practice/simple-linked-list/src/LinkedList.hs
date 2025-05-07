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

-- A simple singly linked list: either Nil or a Node with a value and a next list
data LinkedList a
    = Nil
    | Node a (LinkedList a)
    deriving (Eq, Show)

-- Get the value at the head of the list
datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil        = error "datum: empty list"

-- Build a linked list from a Haskell list
fromList :: [a] -> LinkedList a
fromList = foldr Node Nil

-- Test whether the list is empty
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- Prepend a new element to the front of the list
new :: a -> LinkedList a -> LinkedList a
new = Node

-- Get the tail of the list (everything after the head)
next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil         = error "next: empty list"

-- The empty list
nil :: LinkedList a
nil = Nil

-- Reverse the linked list
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil         = acc
    go acc (Node x xs) = go (Node x acc) xs

-- Convert back to a Haskell list
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Node x xs) = x : toList xs
