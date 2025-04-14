module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals preorder inorder
    | length preorder /= length inorder = Nothing
    | null preorder = Nothing
    | otherwise = buildTree preorder inorder
  where
    buildTree [] _ = Nothing
    buildTree (root:preRest) inord =
        case break (== root) inord of
            (inLeft, _:inRight) ->
                let leftTree = if null inLeft then Nothing else buildTree (take (length inLeft) preRest) inLeft
                    rightTree = if null inRight then Nothing else buildTree (drop (length inLeft) preRest) inRight
                in Just (Node root leftTree rightTree)
            _ -> Nothing
