module Satellite (treeFromTraversals) where

-- Changed import statement:
-- Assuming BinaryTree (type), Empty (value), and Node (function) are exported.
import BinaryTree (BinaryTree, Empty, Node)
import Data.List (elemIndex, splitAt)

-- Assumption from problem: The BinaryTree type conceptually behaves as if defined:
-- data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)
-- where `Empty` represents an empty tree or a missing child.
-- Our change implies that `Empty` and `Node` might be functions/values rather than
-- directly used constructors from the perspective of this module.

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
-- If both traversals are empty, it represents an empty spot (e.g., a missing child).
-- The caller (recursive step) will interpret this Nothing as an Empty tree.
treeFromTraversals [] [] = Nothing

-- If one traversal is empty and the other is not, the inputs are inconsistent.
treeFromTraversals [] _  = Nothing -- Preorder empty, Inorder not
treeFromTraversals _ []  = Nothing -- Preorder not empty, Inorder empty

-- If traversals have different lengths, they are inconsistent.
treeFromTraversals preorder inorder | length preorder /= length inorder = Nothing

-- Main recursive case: preorder and inorder are non-empty and have the same length.
treeFromTraversals (rootVal:preorderRest) inorder =
    -- Find the rootVal in the inorder traversal.
    -- elemIndex requires Eq a, which is implied by Ord a.
    case elemIndex rootVal inorder of
        Nothing -> Nothing -- Root from preorder not found in inorder: inconsistent traversals.
        Just rootIdx ->
            -- Split inorder traversal based on the root's position.
            let (leftInorder, rightInorderWithRoot) = splitAt rootIdx inorder
                -- rightInorderWithRoot starts with rootVal. Elements after it form the right subtree's inorder traversal.
                -- This is safe as elemIndex guarantees rootVal is in inorder, so rightInorderWithRoot is not empty.
                rightInorder = tail rightInorderWithRoot

                -- Determine the number of elements in the left subtree.
                numLeft = length leftInorder

                -- Split the rest of the preorder traversal into parts for left and right subtrees.
                (leftPreorder, rightPreorder) = splitAt numLeft preorderRest
            in
                -- Check for structural consistency: the derived sub-traversals must also have matching lengths.
                if length leftPreorder /= length leftInorder || length rightPreorder /= length rightInorder then
                    Nothing -- Inconsistent structure or element counts.
                else
                    -- Recursively build the left and right subtrees.
                    -- If a sub-traversal is empty (e.g. leftPreorder=[] and leftInorder=[]),
                    -- the recursive call returns Nothing. This Nothing is then converted to
                    -- the `Empty` tree representation.
                    -- If a sub-traversal is inconsistent, Nothing is returned and propagated.
                    let leftChildResult  = treeFromTraversals leftPreorder leftInorder
                        rightChildResult = treeFromTraversals rightPreorder rightInorder

                        -- Convert Maybe (BinaryTree a) to BinaryTree a for children.
                        -- If Nothing, it means an empty spot, so use `Empty`.
                        -- This assumes `Empty` is now in scope (as a value/constant).
                        actualLeftChild  = case leftChildResult of
                                             Just tree -> tree
                                             Nothing   -> Empty -- Represents an empty subtree

                        actualRightChild = case rightChildResult of
                                             Just tree -> tree
                                             Nothing   -> Empty -- Represents an empty subtree
                    in
                        -- Construct the tree node.
                        -- This assumes `Node` is now in scope (as a function).
                        Just (Node rootVal actualLeftChild actualRightChild)
