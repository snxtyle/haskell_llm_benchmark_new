module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (sort)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
  | length preorder /= length inorder = Nothing
  | hasDuplicates sortedPre = Nothing
  | sortedPre /= sort inorder = Nothing
  | otherwise = build preorder inorder
  where
    sortedPre = sort preorder

    hasDuplicates :: Eq a => [a] -> Bool
    hasDuplicates [] = False
    hasDuplicates [_] = False
    hasDuplicates (x:y:xs) = x == y || hasDuplicates (y:xs)

    build :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
    build [] [] = Just Leaf
    build [] _  = Nothing
    build _  [] = Nothing
    build (r:rs) xs =
      case break (== r) xs of
        (leftIn, _:rightIn) ->
          let leftSize = length leftIn
              (leftPre, rightPre) = splitAt leftSize rs
          in do
            leftTree  <- build leftPre leftIn
            rightTree <- build rightPre rightIn
            Just (Branch leftTree r rightTree)
        _ -> Nothing
