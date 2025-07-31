module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (sort)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Null
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | hasDuplicates preorder = Nothing
  | hasDuplicates inorder = Nothing
  | sort preorder /= sort inorder = Nothing
  | otherwise = buildTree preorder inorder
  where
    hasDuplicates xs = length xs /= length (removeDuplicates xs)
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
    
    buildTree [] [] = Just Null
    buildTree (root:preRest) inRest = 
      case break (== root) inRest of
        (_, []) -> Nothing  -- Root not found in inorder
        (leftIn, _:rightIn) -> 
          let leftSize = length leftIn
              (leftPre, rightPre) = splitAt leftSize preRest
          in do
            leftTree <- buildTree leftPre leftIn
            rightTree <- buildTree rightPre rightIn
            return $ Node leftTree root rightTree
    buildTree _ _ = Nothing
