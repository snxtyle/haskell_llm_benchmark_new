module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] _ = Nothing  -- Empty pre-order traversal
treeFromTraversals (root:preorderRest) inorder =
    case elemIndex root inorder of
        Just idx ->
            let leftInorder = take idx inorder  -- Left subtree in-order
                rightInorder = drop (idx + 1) inorder  -- Right subtree in-order
                leftPreorder = take (length leftInorder) preorderRest  -- Left subtree pre-order
                rightPreorder = drop (length leftInorder) preorderRest  -- Right subtree pre-order
            in case (treeFromTraversals leftPreorder leftInorder, treeFromTraversals rightPreorder rightInorder) of
                (Just leftTree, Just rightTree) -> Just (BinaryTree root leftTree rightTree)  -- Successfully built tree
                _ -> Nothing  -- Subtrees could not be built
        Nothing -> Nothing  -- Root not found in in-order traversal
