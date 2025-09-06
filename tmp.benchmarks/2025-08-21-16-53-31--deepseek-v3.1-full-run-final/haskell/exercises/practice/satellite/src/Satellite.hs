module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
    | length preorder /= length inorder = Nothing
    | null preorder = Just Empty
    | otherwise = buildTree preorder inorder

buildTree :: Eq a => [a] -> [a] -> Maybe (BinaryTree a)
buildTree [] [] = Just Empty
buildTree (root:preRest) inorder =
    case elemIndex root inorder of
        Nothing -> Nothing
        Just idx -> do
            let (leftIn, _:rightIn) = splitAt idx inorder
            let leftPre = take (length leftIn) preRest
            let rightPre = drop (length leftIn) preRest
            leftTree <- buildTree leftPre leftIn
            rightTree <- buildTree rightPre rightIn
            return (Branch leftTree root rightTree)
buildTree _ _ = Nothing
