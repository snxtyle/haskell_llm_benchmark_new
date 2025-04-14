module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Leaf
treeFromTraversals [] _  = Nothing
treeFromTraversals _  [] = Nothing
treeFromTraversals (p:ps) inorder =
    case break (== p) inorder of
        (_, []) -> Nothing  -- root not found in inorder
        (leftIn, _:rightIn) ->
            let leftLen = length leftIn
                (leftPre, rightPre) = splitAt leftLen ps
            in do
                leftTree <- treeFromTraversals leftPre leftIn
                rightTree <- treeFromTraversals rightPre rightIn
                return (Branch p leftTree rightTree)
