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

-- Define the BST data type
data BST a = Empty
           | Node a (BST a) (BST a)
           deriving (Eq, Show)

-- Returns the left subtree, or Nothing if the tree is empty
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ left _) = Just left

-- Returns the right subtree, or Nothing if the tree is empty
bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ right) = Just right

-- Returns the value of the node, or Nothing if the tree is empty
bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node value _ _) = Just value

-- Creates an empty BST
empty :: BST a
empty = Empty

-- Creates a BST with a single element
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- Inserts a value into the BST while maintaining the BST property
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node value left right)
  | x <= value = Node value (insert x left) right
  | otherwise  = Node value left (insert x right)

-- Converts a list to a BST by inserting each element
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

-- Converts the BST to a sorted list using in-order traversal
toList :: BST a -> [a]
toList Empty = []
toList (Node value left right) = toList left ++ [value] ++ toList right
