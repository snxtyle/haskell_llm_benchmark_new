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

datum :: LinkedList a -> a
datum (Node x _) = x
datum Nil        = error "datum: empty list"

fromList :: [a] -> LinkedList a
fromList = foldr new nil

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new = Node

next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil         = error "next: empty list"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil         = []
toList (Node x xs) = x : toList xs
