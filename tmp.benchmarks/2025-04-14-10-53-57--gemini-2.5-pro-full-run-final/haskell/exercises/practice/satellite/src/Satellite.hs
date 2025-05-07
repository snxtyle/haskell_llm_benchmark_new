module Satellite (treeFromTraversals) where

-- Explicitly import the type BinaryTree and its constructors Empty and Node.
import BinaryTree (BinaryTree(Empty, Node))
import Data.List (break) -- Used to split the inorder list

-- | Reconstructs a binary tree from its pre-order and in-order traversals.
--
-- Returns Nothing if the traversals are inconsistent (e.g., different lengths,
-- or elements in pre-order not found in in-order).
-- Assumes the tree contains unique elements as per the problem description.
treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty -- Base case: Empty traversals result in an empty tree.
treeFromTraversals [] _  = Nothing  -- Mismatched lengths or inconsistent input.
treeFromTraversals _ []  = Nothing  -- Mismatched lengths or inconsistent input.
treeFromTraversals preorder@(rootVal:preorderTail) inorder
  -- Check if traversals have the same length. This is a basic consistency check.
  | length preorder /= length inorder = Nothing
  | otherwise =
      -- Split the inorder traversal list at the root value.
      -- `leftInorder` contains elements left of the root.
      -- `rootAndRightInorder` starts with the root, followed by elements to its right.
      case break (== rootVal) inorder of
        -- If `break` returns an empty second list, the root was not found in inorder.
        (_, []) -> Nothing -- Inconsistent traversals: root from preorder not in inorder.
        (leftInorder, rootAndRightInorder) ->
          let
            -- Remove the root from the second part to get the right inorder traversal.
            rightInorder = tail rootAndRightInorder

            -- The number of nodes in the left subtree is the length of `leftInorder`.
            -- Split the remaining preorder traversal (`preorderTail`) accordingly.
            (leftPreorder, rightPreorder) = splitAt (length leftInorder) preorderTail
          in
            -- Recursively build the left and right subtrees.
            -- Use applicative style (<$>, <*>) to combine the results within the Maybe context.
            -- If either recursive call returns Nothing, the entire result becomes Nothing.
            Node rootVal
              <$> treeFromTraversals leftPreorder leftInorder
              <*> treeFromTraversals rightPreorder rightInorder

-- Note: The `Ord a` constraint comes from the original function signature.
-- The core logic primarily requires `Eq a` for `break (== rootVal)`.
-- `Ord a` might be required by the `BinaryTree` definition itself or tests,
-- so we keep it.
