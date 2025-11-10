module BinaryTree (BinaryTree(..)) where

-- | A binary tree.
-- It can be either Empty or a Node containing a value,
-- a left subtree, and a right subtree.
data BinaryTree a = Empty
                  | Node a (BinaryTree a) (BinaryTree a)
                  deriving (Eq, Show)
