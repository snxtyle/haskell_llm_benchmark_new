module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals [] _ = Nothing
treeFromTraversals _ [] = Nothing
treeFromTraversals (root:preorderRest) inorder = 
    case elemIndex root inorder of
        Nothing -> Nothing
        Just rootIndex ->
            let (leftInorder, rightInorder) = splitAt rootIndex inorder
                rightInorder' = drop 1 rightInorder  -- Remove the root from right part
                leftSize = length leftInorder
                (leftPreorder, rightPreorder) = splitAt leftSize preorderRest
                buildLeft = treeFromTraversals leftPreorder leftInorder
                buildRight = treeFromTraversals rightPreorder rightInorder'
            in case (buildLeft, buildRight) of
                (Just leftTree, Just rightTree) -> Just (Node leftTree root rightTree)
                _ -> Nothing
