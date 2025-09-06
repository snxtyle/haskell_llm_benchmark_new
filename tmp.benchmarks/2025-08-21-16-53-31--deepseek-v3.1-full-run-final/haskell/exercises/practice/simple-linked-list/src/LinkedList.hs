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

data LinkedList a = Nil | Node a (LinkedList a) deriving (Eq, Show)

-- Get the data from the head of the list
datum :: LinkedList a -> a
datum Nil = error "Cannot get datum from empty list"
datum (Node x _) = x

-- Convert a regular list to a LinkedList
fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Node x (fromList xs)

-- Check if the list is empty
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

-- Create a new node with the given data and next pointer
new :: a -> LinkedList a -> LinkedList a
new = Node

-- Get the next node in the list
next :: LinkedList a -> LinkedList a
next Nil = error "Cannot get next of empty list"
next (Node _ nextNode) = nextNode

-- Create an empty list
nil :: LinkedList a
nil = Nil

-- Reverse the linked list
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = reverseHelper list Nil
  where
    reverseHelper Nil acc = acc
    reverseHelper (Node x xs) acc = reverseHelper xs (Node x acc)

-- Convert a LinkedList to a regular list
toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
