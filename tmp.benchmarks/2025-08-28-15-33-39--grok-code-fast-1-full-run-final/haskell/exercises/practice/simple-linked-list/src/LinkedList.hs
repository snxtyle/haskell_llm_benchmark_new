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
datum Nil = error "Cannot get datum of Nil"

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Node x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = Node x linkedList

next :: LinkedList a -> LinkedList a
next (Node _ xs) = xs
next Nil = error "Cannot get next of Nil"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList list = rev list Nil
  where rev Nil acc = acc
        rev (Node x xs) acc = rev xs (Node x acc)

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
