module Satellite (treeFromTraversals) where

import BinaryTree
import Data.List (sort)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | sort preorder /= sort inorder = Nothing
  | otherwise = build preorder inorder
  where
    build :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
    build [] _ = Just Empty
    build (p:ps) ino =
      case span (/= p) ino of
        (_, []) -> Nothing -- Root from preorder not found in inorder. Should not happen with the guard.
        (leftInorder, _:rightInorder) ->
          let (leftPreorder, rightPreorder) = splitAt (length leftInorder) ps
          in Node p <$> build leftPreorder leftInorder <*> build rightPreorder rightInorder
