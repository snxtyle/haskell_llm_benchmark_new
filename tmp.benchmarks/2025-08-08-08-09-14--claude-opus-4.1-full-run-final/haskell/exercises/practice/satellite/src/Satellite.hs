module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(Node))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Nothing
treeFromTraversals [x] [y] 
  | x == y = Just (Node x Nothing Nothing)
  | otherwise = Nothing
treeFromTraversals (root:preRest) inorder =
  case splitAt' root inorder of
    Nothing -> Nothing
    Just (leftIn, rightIn) ->
      let leftSize = length leftIn
          (leftPre, rightPre) = splitAt leftSize preRest
          leftTree = if null leftIn 
                     then Nothing 
                     else treeFromTraversals leftPre leftIn
          rightTree = if null rightIn 
                      then Nothing 
                      else treeFromTraversals rightPre rightIn
      in Just (Node root leftTree rightTree)
treeFromTraversals _ _ = Nothing

-- Helper function to split a list at a specific element
-- Returns Nothing if element not found, otherwise (before, after)
splitAt' :: Eq a => a -> [a] -> Maybe ([a], [a])
splitAt' _ [] = Nothing
splitAt' x (y:ys)
  | x == y = Just ([], ys)
  | otherwise = do
      (before, after) <- splitAt' x ys
      return (y:before, after)
