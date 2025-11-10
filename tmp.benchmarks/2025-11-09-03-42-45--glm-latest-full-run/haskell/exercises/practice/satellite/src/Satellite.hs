module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

-- | Reconstructs a binary tree from its pre-order and in-order traversals.
-- Returns Nothing if the traversals are invalid (e.g., different lengths,
-- elements don't match, or a root is not found in the in-order traversal).
treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  -- Basic validation: traversals must have the same length.
  | length preorder /= length inorder = Nothing
  -- Base case: empty traversals mean an empty tree.
  | null preorder = Just Empty
  | otherwise = do
      let rootVal = head preorder
      -- Find the root's position in the in-order traversal.
      -- If not found, elemIndex returns Nothing, and the do-block short-circuits.
      rootIndex <- elemIndex rootVal inorder
      
      -- Split the traversals for the left and right subtrees.
      let leftInorder = take rootIndex inorder
      let rightInorder = drop (rootIndex + 1) inorder
      
      let leftPreorder = take (length leftInorder) (tail preorder)
      let rightPreorder = drop (length leftInorder) (tail preorder)
      
      -- Recursively build the subtrees.
      leftTree <- treeFromTraversals leftPreorder leftInorder
      rightTree <- treeFromTraversals rightPreorder rightInorder
      
      -- Construct the current node.
      return (Node rootVal leftTree rightTree)
