module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

-- Define the BST data type.
-- A BST can either be an EmptyNode or a Node containing a value, a left subtree, and a right subtree.
data BST a = EmptyNode | Node a (BST a) (BST a) deriving (Eq, Show)

-- Returns the left subtree of a given BST.
-- Returns Nothing if the tree is empty.
bstLeft :: BST a -> Maybe (BST a)
bstLeft EmptyNode = Nothing
bstLeft (Node _ left _) = Just left

-- Returns the right subtree of a given BST.
-- Returns Nothing if the tree is empty.
bstRight :: BST a -> Maybe (BST a)
bstRight EmptyNode = Nothing
bstRight (Node _ _ right) = Just right

-- Returns the value of the current node in a BST.
-- Returns Nothing if the tree is empty.
bstValue :: BST a -> Maybe a
bstValue EmptyNode = Nothing
bstValue (Node val _ _) = Just val

-- Creates an empty BST.
empty :: BST a
empty = EmptyNode

-- Constructs a BST from a list of elements.
-- Elements are inserted one by one into an initially empty tree.
fromList :: Ord a => [a] -> BST a
fromList xs = foldl insert empty xs -- Changed from foldr to foldl

-- Inserts an element into a BST.
-- If the tree is empty, creates a new node.
-- If the element is less than or equal to the current node's value, it's inserted into the left subtree.
-- Otherwise, it's inserted into the right subtree.
insert :: Ord a => a -> BST a -> BST a
insert x EmptyNode = singleton x
insert x (Node val left right)
    | x <= val  = Node val (insert x left) right
    | otherwise = Node val left (insert x right)

-- Creates a BST with a single node.
singleton :: a -> BST a
singleton x = Node x EmptyNode EmptyNode

-- Converts a BST into a sorted list using an in-order traversal.
-- Traverses the left subtree, then adds the current node's value, then traverses the right subtree.
toList :: BST a -> [a]
toList EmptyNode = []
toList (Node val left right) = toList left ++ [val] ++ toList right
