module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree (..))
import Control.Monad (guard)

-- | Reconstruct a binary tree from its preorder and inorder traversals.
--
--   Algorithm:
--     • the first element of the preorder list is the root
--     • find this root in the inorder list:
--         – everything to the left belongs to the left subtree
--         – everything to the right belongs to the right subtree
--     • knowing the size of the left in‑order part lets us split the
--       remaining preorder list into the corresponding left / right parts
--     • recurse for both sub‑trees
--
--   ‘Nothing’ is returned whenever the traversals are inconsistent:
--     • different lengths
--     • the root from preorder is missing in the inorder list
--     • the implied subtree lengths don’t match
treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | otherwise                         = build preorder inorder
  where
    build [] [] = Just Leaf
    build [] _  = Nothing
    build _  [] = Nothing
    build (r:ps) xs = do
      let (leftIn, rest) = break (== r) xs
      -- root must appear in the inorder list
      rightIn <- case rest of
                   []      -> Nothing
                   (_:rs)  -> Just rs
      let (leftPre, rightPre) = splitAt (length leftIn) ps
      guard (length leftPre == length leftIn)
      leftTree  <- build leftPre  leftIn
      rightTree <- build rightPre rightIn
      pure $ Branch leftTree r rightTree
