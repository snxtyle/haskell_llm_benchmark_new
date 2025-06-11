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

data BST a = Empty | Node a (BST a) (BST a)
    deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ left _) = Just left

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = Just right

bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node x _ _) = Just x

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node y left right)
    | x <= y    = Node y (insert x left) right
    | otherwise = Node y left (insert x right)

singleton :: a -> BST a
singleton x = Node x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Node x left right) = toList left ++ [x] ++ toList right
