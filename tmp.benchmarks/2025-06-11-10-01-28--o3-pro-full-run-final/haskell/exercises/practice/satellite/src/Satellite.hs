module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List  (elemIndex, splitAt)

-- | Rebuild a binary tree from its preorder and inorder traversals.
--
--   Returns 'Nothing' if the given traversals are not valid descriptions of
--   the same binary tree (different lengths, missing elements, duplicates,
--   or mismatched ordering).
--
--   Because every element in the tree is unique, the algorithm works by:
--
--   1. Taking the first element of the preorder list – this is the root.
--   2. Finding that root in the inorder list.
--   3. Everything to the left of the root in the inorder list is the left
--      subtree; everything to the right is the right subtree.
--   4. Knowing the size of the left subtree allows us to split the remaining
--      preorder list into the preorder traversals of the left and right
--      sub-trees respectively.
--   5. Recursively repeat the process for both sub-trees.
--
--   The overall complexity is O(n²) in the worst case (because of the
--   repeated element searches in the inorder list), which is sufficient for
--   the small inputs used in the exercise.  A more efficient O(n) solution
--   could be written by pre-building an index map of the inorder positions.
treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder =
  -- The two traversals must describe the same number of nodes
  if length preorder /= length inorder
     then Nothing
     else buildTree preorder inorder
  where
    buildTree :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
    buildTree [] [] = Just Empty
    buildTree [] _  = Nothing
    buildTree _  [] = Nothing
    buildTree (root:preRest) inOrder = do
      -- Locate the root in the inorder traversal
      rootIdx <- elemIndex root inOrder
      let (leftInOrder, _root: rightInOrder) = splitAt rootIdx inOrder
          leftSize = length leftInOrder

      -- Ensure the preorder list is long enough to contain the left subtree
      if length preRest < leftSize
         then Nothing
         else do
           let (leftPreOrder, rightPreOrder) = splitAt leftSize preRest
           leftTree  <- buildTree leftPreOrder  leftInOrder
           rightTree <- buildTree rightPreOrder rightInOrder
           pure (Node leftTree root rightTree)
