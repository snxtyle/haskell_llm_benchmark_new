module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Nothing
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | not (sameElements preorder inorder) = Nothing
  | otherwise = buildTree preorder inorder
  where
    sameElements xs ys = all (`elem` ys) xs && all (`elem` xs) ys
    
    buildTree [] [] = Nothing
    buildTree [x] [y]
      | x == y = Just (Branch Leaf x Leaf)
      | otherwise = Nothing
    buildTree (root:restPre) inorder' =
      case splitAt rootIndex inorder' of
        (leftInorder, _:rightInorder) ->
          let leftSize = length leftInorder
              (leftPreorder, rightPreorder) = splitAt leftSize restPre
              leftTree = if null leftPreorder then Just Leaf else buildTree leftPreorder leftInorder
              rightTree = if null rightPreorder then Just Leaf else buildTree rightPreorder rightInorder
          in case (leftTree, rightTree) of
               (Just left, Just right) -> Just (Branch left root right)
               _ -> Nothing
        _ -> Nothing
      where
        rootIndex = findIndex root inorder' 0
        
    findIndex _ [] _ = -1
    findIndex x (y:ys) i
      | x == y = i
      | otherwise = findIndex x ys (i + 1)
