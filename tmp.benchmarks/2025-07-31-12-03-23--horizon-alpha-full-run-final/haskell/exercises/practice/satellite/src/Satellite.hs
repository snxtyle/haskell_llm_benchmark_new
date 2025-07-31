module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import qualified Data.Set as Set
import qualified Data.List as List

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder = do
  -- Basic sanity checks
  let nPre = length preorder
      nIn  = length inorder
  if nPre /= nIn then Nothing else do
    -- Ensure both traversals contain the same unique items
    let setPre = Set.fromList preorder
        setIn  = Set.fromList inorder
    if Set.size setPre /= nPre || Set.size setIn /= nIn then Nothing else do
      if setPre /= setIn then Nothing else
        build preorder inorder
  where
    build [] [] = Just Leaf
    build (p:ps) inorder' = do
      -- Find root in inorder traversal
      (leftIn, rightIn) <- splitOn p inorder'
      let leftSize = length leftIn
          (leftPre, rightPre) = List.splitAt leftSize ps
      leftTree  <- build leftPre leftIn
      rightTree <- build rightPre rightIn
      pure (Node leftTree p rightTree)
    build _ _ = Nothing

    -- Split a list into the portion left of a pivot and the portion right of it.
    -- Returns Nothing if the pivot is not found.
    splitOn :: Eq a => a -> [a] -> Maybe ([a], [a])
    splitOn x xs = case break (== x) xs of
      (l, _ : r) -> Just (l, r)
      _          -> Nothing
