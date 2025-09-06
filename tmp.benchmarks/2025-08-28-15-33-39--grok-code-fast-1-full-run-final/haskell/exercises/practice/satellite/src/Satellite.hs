module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (sort, elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | sort preorder /= sort inorder = Nothing
  | otherwise = buildTree preorder inorder

buildTree :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
buildTree [] [] = Just Leaf
buildTree (root:restPre) inord =
  case elemIndex root inord of
    Nothing -> Nothing
    Just idx ->
      let leftIn = take idx inord
          rightIn = drop (idx + 1) inord
          leftPreLen = length leftIn
          leftPre = take leftPreLen restPre
          rightPre = drop leftPreLen restPre
      in do
        leftTree <- buildTree leftPre leftIn
        rightTree <- buildTree rightPre rightIn
        return (Branch leftTree root rightTree)
buildTree _ _ = Nothing
