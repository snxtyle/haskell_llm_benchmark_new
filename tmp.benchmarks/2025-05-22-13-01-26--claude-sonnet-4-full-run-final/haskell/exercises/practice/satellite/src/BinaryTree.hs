module BinaryTree (BinaryTree(..)) where

data BinaryTree a = Empty | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show, Eq)
