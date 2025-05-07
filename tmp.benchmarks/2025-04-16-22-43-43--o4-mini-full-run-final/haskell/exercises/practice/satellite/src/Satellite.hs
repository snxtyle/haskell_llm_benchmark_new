module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (p:ps) inorder =
  case elemIndex p inorder of
    Nothing   -> Nothing
    Just idx  ->
      let (inL, _ : inR)    = splitAt idx inorder
          (preL, preR)      = splitAt (length inL) ps
      in case ( treeFromTraversals preL inL
              , treeFromTraversals preR inR ) of
           (Just left, Just right) -> Just (Node p left right)
           _                       -> Nothing
