module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a
    = Empty
    | Node a (BST a) (BST a)
    deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty              = Nothing
bstLeft (Node _ leftSub _) = Just leftSub

bstRight :: BST a -> Maybe (BST a)
bstRight Empty               = Nothing
bstRight (Node _ _ rightSub) = Just rightSub

bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node v _ _) = Just v

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node v l r)
  | x <= v    = Node v (insert x l) r
  | otherwise = Node v l (insert x r)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty        = []
toList (Node v l r) = toList l ++ [v] ++ toList r
