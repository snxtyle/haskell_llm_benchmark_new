module BinaryTree (BinaryTree (..)) where

-- | A simple binary tree.
--   ‘Leaf’ represents an empty tree.
--   ‘Branch left value right’ stores a node value together with its left
--   and right sub‑trees.
data BinaryTree a
  = Leaf
  | Branch (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)
