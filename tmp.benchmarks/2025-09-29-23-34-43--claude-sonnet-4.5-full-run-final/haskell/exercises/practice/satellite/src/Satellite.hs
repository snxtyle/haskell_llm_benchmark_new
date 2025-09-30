module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(Leaf, Branch))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Leaf
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals (p:ps) inorder
  | length (p:ps) /= length inorder = Nothing
  | otherwise = case elemIndex p inorder of
      Nothing -> Nothing
      Just rootIdx ->
        let leftInorder = take rootIdx inorder
            rightInorder = drop (rootIdx + 1) inorder
            leftSize = length leftInorder
            leftPreorder = take leftSize ps
            rightPreorder = drop leftSize ps
        in do
          leftTree <- treeFromTraversals leftPreorder leftInorder
          rightTree <- treeFromTraversals rightPreorder rightInorder
          return (Branch leftTree p rightTree)
