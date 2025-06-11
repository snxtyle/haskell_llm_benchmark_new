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

-- | A simple singly-linked list.
--   Either an empty list (Nil) or a value followed by the rest of the list.
data LinkedList a = Nil | Node a (LinkedList a)
    deriving (Eq, Show)

-- | Retrieve the element stored in the head of the list.
--   Errors if the list is empty.
datum :: LinkedList a -> a
datum Nil          = error "Cannot take datum of an empty list."
datum (Node x  _)  = x

-- | Build a linked list from a regular Haskell list.
fromList :: [a] -> LinkedList a
fromList = foldr Node Nil

-- | Check whether the list is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- | Prepend an element to a linked list, producing a new list.
new :: a -> LinkedList a -> LinkedList a
new = Node

-- | Return the list representing everything after the head element.
--   Errors if the list is empty.
next :: LinkedList a -> LinkedList a
next Nil          = error "Cannot take next of an empty list."
next (Node _ xs)  = xs

-- | The empty linked list value.
nil :: LinkedList a
nil = Nil

-- | Reverse a linked list.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go :: LinkedList a -> LinkedList a -> LinkedList a
    go acc Nil         = acc
    go acc (Node x xs) = go (Node x acc) xs

-- | Convert a linked list back into a regular Haskell list.
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Node x xs) = x : toList xs
