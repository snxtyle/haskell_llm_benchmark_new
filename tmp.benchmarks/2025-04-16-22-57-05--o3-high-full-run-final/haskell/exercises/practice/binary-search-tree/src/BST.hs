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

-- | A simple binary search tree.  An `Empty` constructor represents an
-- empty tree, while `Node value left right` stores a value together
-- with its left and right sub‑trees.
data BST a
    = Empty
    | Node a (BST a) (BST a)
    deriving (Eq, Show)

-- | Retrieve the left sub‑tree, if it exists.  
--   Even when the sub‑tree is `Empty`, we still wrap it in `Just`
--   so callers can distinguish between “there is no current node”
--   and “the current node has an empty left branch”.
bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty          = Nothing
bstLeft (Node _ l _)   = Just l

-- | Retrieve the right sub‑tree, if it exists.  
--   Mirrors the behaviour of 'bstLeft'.
bstRight :: BST a -> Maybe (BST a)
bstRight Empty         = Nothing
bstRight (Node _ _ r ) = Just r

-- | Retrieve the value stored at the current node.
bstValue :: BST a -> Maybe a
bstValue Empty        = Nothing
bstValue (Node v _ _) = Just v

-- | An empty binary search tree.
empty :: BST a
empty = Empty

-- | Create a tree containing a single element.
singleton :: a -> BST a
singleton x = Node x Empty Empty

-- | Insert a value into the tree, preserving the BST ordering rule.
--   Duplicate values are inserted into the left sub‑tree.
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node v l r)
    | x <= v    = Node v (insert x l) r
    | otherwise = Node v l (insert x r)

-- | Build a tree from a list of values.
fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) Empty

-- | Convert the tree to an ordered list via in‑order traversal.
toList :: BST a -> [a]
toList Empty        = []
toList (Node v l r) = toList l ++ [v] ++ toList r
