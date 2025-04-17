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

-- | A simple, singly‑linked list.
data LinkedList a
    = Nil                     -- ^ Empty list
    | Node a (LinkedList a)   -- ^ Element and pointer to the next node
    deriving (Eq, Show)

-- | The empty list.
nil :: LinkedList a
nil = Nil

-- | Check whether a list is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- | Create a new node with a value and a pointer to the rest of the list.
new :: a -> LinkedList a -> LinkedList a
new = Node

-- | Extract the element stored in the current node.
--   Throws an error if the list is empty.
datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil        = error "Cannot take datum of an empty list"

-- | Obtain the rest of the list after the current node.
--   Throws an error if the list is empty.
next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil         = error "Cannot take next of an empty list"

-- | Convert a standard Haskell list to a LinkedList.
fromList :: [a] -> LinkedList a
fromList = foldr Node Nil

-- | Convert a LinkedList back into a standard list.
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Node x xs) = x : toList xs

-- | Reverse a LinkedList.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil         = acc
    go acc (Node x xs) = go (Node x acc) xs
