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

-- | Binary Search Tree definition.
-- An empty tree is represented by 'Empty', while a populated node is
-- represented by @Node left value right@.
data BST a
    = Empty
    | Node
        { leftSub  :: BST a   -- ^ left  subtree (values ≤ node value)
        , nodeVal  :: a       -- ^ value stored at this node
        , rightSub :: BST a   -- ^ right subtree (values  > node value)
        }
    deriving (Eq, Show)

-- | Return the left subtree of a node.
--   • If the tree itself is empty,      returns Nothing.
--   • Otherwise returns @Just leftTree@.  Note that the left subtree
--     may itself be 'Empty'.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty          = Nothing
bstLeft (Node l _ _)   = Just l

-- | Return the right subtree of a node.
--   Follows the same rules as 'bstLeft'.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty         = Nothing
bstRight (Node _ _ r)  = Just r

-- | Return the value stored at the root of the tree.
--   • For an empty tree, returns Nothing.
--   • Otherwise, returns @Just value@.
bstValue :: BST a -> Maybe a
bstValue Empty         = Nothing
bstValue (Node _ v _)  = Just v

-- | An empty binary‑search tree.
empty :: BST a
empty = Empty

-- | Create a singleton tree containing a single value.
singleton :: a -> BST a
singleton x = Node Empty x Empty

-- | Insert a value into the tree, preserving the BST property.
--   Values less than *or equal* to the current node are inserted
--   into the left subtree; larger values go into the right subtree.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node l v r)
    | x <= v    = Node (insert x l) v r
    | otherwise = Node l v (insert x r)

-- | Build a tree from a list of values by successive insertion.
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

-- | Convert the tree to an ascending list via an in‑order traversal.
toList :: BST a -> [a]
toList Empty         = []
toList (Node l v r)  = toList l ++ [v] ++ toList r
