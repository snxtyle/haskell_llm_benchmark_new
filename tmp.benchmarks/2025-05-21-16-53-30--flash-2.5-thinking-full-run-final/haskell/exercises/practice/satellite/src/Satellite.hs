module Satellite (treeFromTraversals) where

-- import BinaryTree (BinaryTree(..)) -- Commented out as BinaryTree is now defined locally
import Data.List (findIndex)

-- Define BinaryTree data type locally to ensure constructors are in scope.
-- This is a workaround. The ideal fix would be to ensure BinaryTree.hs
-- correctly exports its constructors (Empty and Node).
-- This might lead to duplicate definition errors if BinaryTree.hs also defines
-- this type and is compiled.
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq, Ord)

-- | Reconstructs a binary tree from its pre-order and in-order traversals.
-- Returns 'Nothing' if the traversals are inconsistent or cannot form a valid tree.
treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals [] [] = Just Empty
treeFromTraversals _  [] = Nothing -- Inorder is empty, but preorder is not (invalid input)
treeFromTraversals [] _  = Nothing -- Preorder is empty, but inorder is not (invalid input)
treeFromTraversals (rootVal:preRest) inorder =
    case findIndex (== rootVal) inorder of
        Nothing -> Nothing -- Root value from preorder not found in inorder (inconsistent traversals)
        Just idx ->
            let
                -- Elements in inorder before the root form the left subtree's inorder traversal
                leftInorder  = take idx inorder
                -- Elements in inorder after the root form the right subtree's inorder traversal
                rightInorder = drop (idx + 1) inorder

                -- The number of elements in the left subtree's inorder traversal
                lenLeft      = length leftInorder

                -- The first 'lenLeft' elements of the remaining preorder list belong to the left subtree
                leftPreorder = take lenLeft preRest
                -- The rest of the elements in the remaining preorder list belong to the right subtree
                rightPreorder = drop lenLeft preRest
            in
                -- Recursively build the left and right subtrees
                case (treeFromTraversals leftPreorder leftInorder,
                      treeFromTraversals rightPreorder rightInorder) of
                    (Just leftTree, Just rightTree) -> Just (Node rootVal leftTree rightTree)
                    -- If either subtree fails to build (returns Nothing), then the overall tree cannot be built
                    _ -> Nothing
