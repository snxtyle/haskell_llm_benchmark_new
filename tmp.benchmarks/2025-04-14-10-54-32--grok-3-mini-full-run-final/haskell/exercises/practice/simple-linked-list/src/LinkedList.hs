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

-- Define the singly linked list data type
data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

-- Returns the datum (value) of the current node
datum :: LinkedList a -> a
datum (Cons x _) = x  -- Return the head of the Cons cell
datum Nil = error "Cannot get datum from an empty list"  -- Error for empty list

-- Converts a Haskell list to a LinkedList
fromList :: [a] -> LinkedList a
fromList [] = Nil  -- Empty list becomes Nil
fromList (x:xs) = Cons x (fromList xs)  -- Recursively build the list

-- Checks if the LinkedList is empty
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

-- Creates a new node with a value and a next LinkedList
new :: a -> LinkedList a -> LinkedList a
new x nextList = Cons x nextList  -- Simply create a new Cons cell

-- Returns the next LinkedList in the chain
next :: LinkedList a -> LinkedList a
next (Cons _ rest) = rest  -- Return the rest of the list
next Nil = error "Cannot get next from an empty list"  -- Error for empty list

-- Returns an empty LinkedList
nil :: LinkedList a
nil = Nil

-- Reverses the LinkedList
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = rev list Nil
  where
    rev Nil acc = acc  -- Base case: empty list, return accumulator
    rev (Cons x xs) acc = rev xs (Cons x acc)  -- Recur: add current element to accumulator

-- Converts a LinkedList to a Haskell list
toList :: LinkedList a -> [a]
toList Nil = []  -- Empty list becomes empty Haskell list
toList (Cons x xs) = x : toList xs  -- Recursively build the list
