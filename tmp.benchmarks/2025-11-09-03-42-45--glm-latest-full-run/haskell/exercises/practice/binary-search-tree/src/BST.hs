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

-- | A binary search tree.
-- It is either Empty or a Node containing a value, a left subtree, and a right subtree.
data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

-- | Returns the left subtree of a tree node.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ left _) = Just left

-- | Returns the right subtree of a tree node.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = Just right

-- | Returns the value at the root of the tree.
bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node value _ _) = Just value

-- | Creates an empty tree.
empty :: BST a
empty = Empty

-- | Creates a tree from a list of values.
fromList :: Ord a => [a] -> BST a
fromList = foldr insert empty

-- | Inserts a value into a binary search tree.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node v left right)
    | x <= v    = Node v (insert x left) right
    | otherwise = Node v left (insert x right)

-- | Creates a tree with a single value.
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- | Converts a tree to a list using an in-order traversal.
toList :: BST a -> [a]
toList Empty = []
toList (Node v left right) = toList left ++ [v] ++ toList right
