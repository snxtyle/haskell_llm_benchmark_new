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
datum Nil = error "Cannot get datum from empty list"
datum (Node x _) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next Nil = error "Cannot get next from empty list"
next (Node _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList linkedList = reverseHelper linkedList Nil
  where
    reverseHelper Nil acc = acc
    reverseHelper (Node x xs) acc = reverseHelper xs (Node x acc)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
