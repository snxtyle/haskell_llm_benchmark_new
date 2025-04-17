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

-- | A simple singly‑linked list.
--   We keep the constructors private so the list
--   can only be manipulated through the provided API.
data LinkedList a
    = Nil
    | Cons a (LinkedList a)
    deriving (Eq, Show)

-- | Retrieve the element stored in the head node.
--   Calling 'datum' on an empty list is an error because
--   there is no sensible value to return.
datum :: LinkedList a -> a
datum Nil          = error "Cannot obtain datum from an empty list."
datum (Cons x _)   = x

-- | Build a linked list from a regular list.
fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

-- | Check whether the linked list is empty.
isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

-- | Prepend a new element to the list.
new :: a -> LinkedList a -> LinkedList a
new = Cons

-- | Return the remainder of the list after the head element.
--   Calling 'next' on an empty list is an error because
--   there is no next node.
next :: LinkedList a -> LinkedList a
next Nil          = error "Cannot obtain next from an empty list."
next (Cons _ xs)  = xs

-- | The empty list value.
nil :: LinkedList a
nil = Nil

-- | Reverse a linked list.
reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = rev Nil
  where
    rev acc Nil         = acc
    rev acc (Cons x xs) = rev (Cons x acc) xs

-- | Convert a linked list back to a regular list.
toList :: LinkedList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs
