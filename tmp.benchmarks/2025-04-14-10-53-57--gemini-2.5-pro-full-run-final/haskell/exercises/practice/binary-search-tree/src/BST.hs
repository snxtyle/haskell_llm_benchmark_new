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

import Data.Foldable (foldl') -- Using foldl' for potentially better performance

-- A Binary Search Tree is either Empty or a Node containing a value 'a',
-- a left subtree (BST a), and a right subtree (BST a).
data BST a = Empty | Node a (BST a) (BST a) deriving (Eq, Show)

-- Returns the left subtree.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Node _ l _) = Just l

-- Returns the right subtree.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Node _ _ r) = Just r

-- Returns the value of the root node.
bstValue :: BST a -> Maybe a
bstValue Empty = Nothing
bstValue (Node v _ _) = Just v

-- Returns an empty BST.
empty :: BST a
empty = Empty

-- Builds a BST from a list of elements.
-- Uses foldl' to insert elements one by one into the growing tree.
fromList :: Ord a => [a] -> BST a
fromList = foldl' (flip insert) empty

-- Inserts an element into the BST.
-- If the tree is Empty, creates a new singleton node.
-- If the element is <= the node's value, inserts into the left subtree.
-- Otherwise, inserts into the right subtree.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node v l r)
    | x <= v    = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

-- Creates a BST with a single node.
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- Converts the BST to a list using in-order traversal.
-- Left subtree -> Node value -> Right subtree
toList :: BST a -> [a]
toList Empty = []
toList (Node v l r) = toList l ++ [v] ++ toList r
