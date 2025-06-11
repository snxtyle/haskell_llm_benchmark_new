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

-- | A simple binary search tree.
-- An empty tree is represented by 'Empty'.
-- A non-empty tree is a 'Node' that contains a left subtree,
-- a value, and a right subtree.
data BST a
    = Empty
    | Node (BST a) a (BST a)
    deriving (Eq, Show)

-- | Get the left subtree of a node, if it exists.
--   Returns 'Nothing' only when the supplied tree itself is empty.
--   Even when the left branch is empty, it is returned wrapped in 'Just'.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty            = Nothing
bstLeft (Node left _ _)  = Just left

-- | Get the right subtree of a node, if it exists.
--   Returns 'Nothing' only when the supplied tree itself is empty.
--   Even when the right branch is empty, it is returned wrapped in 'Just'.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty             = Nothing
bstRight (Node _ _ right)  = Just right

-- | Get the value stored in a node, if it exists.
bstValue :: BST a -> Maybe a
bstValue Empty         = Nothing
bstValue (Node _ v _)  = Just v

-- | The empty binary search tree.
empty :: BST a
empty = Empty

-- | Create a tree that contains a single value.
singleton :: a -> BST a
singleton x = Node Empty x Empty

-- | Insert a value into the tree, preserving the BST property.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = singleton x
insert x (Node left v right)
    | x <= v    = Node (insert x left) v right
    | otherwise = Node left v (insert x right)

-- | Build a BST from a list of values by repeated insertion.
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Empty

-- | Convert a BST to a list by an in-order traversal.
toList :: BST a -> [a]
toList Empty              = []
toList (Node left v right) = toList left ++ [v] ++ toList right
