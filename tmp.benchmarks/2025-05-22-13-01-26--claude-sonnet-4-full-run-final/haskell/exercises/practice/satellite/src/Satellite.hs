module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals (rootVal:preRest) inorder = 
  case findIndex rootVal inorder of
    Nothing -> Nothing
    Just rootIdx -> 
      let (leftInorder, rightInorder) = splitAt rootIdx inorder
          leftSize = length leftInorder
          (leftPreorder, rightPreorder) = splitAt leftSize preRest
      in do
        leftSubtree <- treeFromTraversals leftPreorder leftInorder
        rightSubtree <- treeFromTraversals rightPreorder rightInorder
        return $ Node leftSubtree rootVal rightSubtree

findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex x xs = findIndex' x xs 0
  where
    findIndex' _ [] _ = Nothing
    findIndex' y (z:zs) i
      | y == z = Just i
      | otherwise = findIndex' y zs (i + 1)
