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

-- Define the Binary Search Tree data type.
-- It can be either Empty or a Node containing a value, a left subtree, and a right subtree.
-- Deriving Eq and Show to allow comparison and printing of BSTs.
data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

-- Returns the left subtree of a BST.
-- If the tree is Empty, returns Nothing.
-- Otherwise, returns Just the left subtree.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ l _) = Just l

-- Returns the right subtree of a BST.
-- If the tree is Empty, returns Nothing.
-- Otherwise, returns Just the right subtree.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ r) = Just r

-- Returns the value at the root of a BST.
-- If the tree is Empty, returns Nothing.
-- Otherwise, returns Just the value at the root node.
bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node v _ _) = Just v

-- Returns an empty BST.
empty :: BST a
empty = Empty

-- Constructs a BST from a list of elements.
-- Elements are inserted one by one into an initially empty tree.
-- Requires Ord a for comparison during insertion.
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

-- Inserts a value into a BST.
-- If the tree is Empty, creates a new singleton tree with the value.
-- If the value is less than or equal to the current node's value,
-- it's inserted into the left subtree.
-- Otherwise, it's inserted into the right subtree.
-- Requires Ord a for comparison.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node v l r)
    | x <= v    = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

-- Creates a BST with a single node containing the given value.
-- The left and right subtrees are Empty.
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- Converts a BST to a list of its elements in sorted order (in-order traversal).
-- If the tree is Empty, returns an empty list.
-- Otherwise, recursively converts the left subtree, appends the current node's value,
-- and then appends the result of recursively converting the right subtree.
toList :: BST a -> [a]
toList Empty = []
toList (Node v l r) = toList l ++ [v] ++ toList r
