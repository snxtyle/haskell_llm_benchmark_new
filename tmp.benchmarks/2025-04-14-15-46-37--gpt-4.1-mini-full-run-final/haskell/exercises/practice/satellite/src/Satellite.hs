module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(Empty, Node))

treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Nothing
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | null preorder = Nothing
  | otherwise =
      let root = head preorder
          (leftInorder, rest) = break (== root) inorder
      in case rest of
           [] -> Nothing  -- root not found in inorder
           (_:rightInorder) ->
             let leftPreorder = take (length leftInorder) (tail preorder)
                 rightPreorder = drop (length leftInorder) (tail preorder)
                 leftSubtree = treeFromTraversals leftPreorder leftInorder
                 rightSubtree = treeFromTraversals rightPreorder rightInorder
             in Just (Node root (maybe Empty id leftSubtree) (maybe Empty id rightSubtree))
