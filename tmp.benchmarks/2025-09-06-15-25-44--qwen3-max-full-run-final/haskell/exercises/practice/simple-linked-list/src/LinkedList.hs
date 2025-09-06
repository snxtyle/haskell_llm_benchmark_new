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
datum Nil = error "Empty list has no datum"

fromList :: [a] -> LinkedList a
fromList [] = nil
fromList (x:xs) = new x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x nextNode = Node x nextNode

next :: LinkedList a -> LinkedList a
next (Node _ nextNode) = nextNode
next Nil = error "Empty list has no next"

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = reverseHelper nil
  where
    reverseHelper acc Nil = acc
    reverseHelper acc (Node x xs) = reverseHelper (new x acc) xs

toList :: LinkedList a -> [a]
toList Nil = []
toList (Node x xs) = x : toList xs
