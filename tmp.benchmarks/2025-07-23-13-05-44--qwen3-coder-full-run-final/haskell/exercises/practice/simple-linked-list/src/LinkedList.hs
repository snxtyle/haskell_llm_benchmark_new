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

data LinkedList a = Nil | Cons a (LinkedList a) deriving (Eq, Show)

datum :: LinkedList a -> a
datum Nil = error "Empty list"
datum (Cons x _) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new x xs = Cons x xs

next :: LinkedList a -> LinkedList a
next Nil = error "Empty list"
next (Cons _ xs) = xs

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs
