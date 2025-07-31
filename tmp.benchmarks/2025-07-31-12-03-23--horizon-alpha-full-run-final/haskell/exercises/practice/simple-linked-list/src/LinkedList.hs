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

-- A simple singly linked list with an explicit empty constructor.
data LinkedList a
    = Empty
    | Node a (LinkedList a)
    deriving (Eq, Show)

-- Extract the value at the head of the list.
-- This is partial for Empty, mirroring common linked list APIs.
datum :: LinkedList a -> a
datum (Node x _) = x
datum Empty      = error "datum: empty linked list"

-- Build a LinkedList from a regular Haskell list.
fromList :: [a] -> LinkedList a
fromList = foldr Node Empty

-- Check if the list is empty.
isNil :: LinkedList a -> Bool
isNil Empty      = True
isNil (Node _ _) = False

-- Construct a new node with value x pointing to the given list.
new :: a -> LinkedList a -> LinkedList a
new = Node

-- Get the tail of the list.
-- This is partial for Empty, mirroring common linked list APIs.
next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Empty       = error "next: empty linked list"

-- The empty linked list.
nil :: LinkedList a
nil = Empty

-- Reverse the linked list.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Empty
  where
    go acc Empty      = acc
    go acc (Node x t) = go (Node x acc) t

-- Convert a LinkedList back into a regular Haskell list.
toList :: LinkedList a -> [a]
toList Empty      = []
toList (Node x t) = x : toList t
