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
datum Nil = error "datum called on empty list"

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x xs = Node x xs

next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil = error "next called on empty list"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = go Nil
  where
    go acc Nil = acc
    go acc (Node x xs) = go (Node x acc) xs

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
