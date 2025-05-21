module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- Represents the context of a parent node when moving down the tree.
-- LContext: We came from the left child; 'rightSibling' is the parent's right child.
-- RContext: We came from the right child; 'leftSibling' is the parent's left child.
data Context a = LContext { parentValue  :: a
                          , rightSibling :: Maybe (BinTree a)
                          }
               | RContext { parentValue  :: a
                          , leftSibling  :: Maybe (BinTree a)
                          } deriving (Eq, Show)

-- The Zipper data structure.
-- 'focus' is the current node being pointed to.
-- 'parents' is a list of contexts, representing the path back to the root.
data Zipper a = Zipper { focus   :: BinTree a
                       , parents :: [Context a]
                       } deriving (Eq, Show)

-- Creates a zipper from a binary tree, with the focus on the root.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Converts a zipper back into a binary tree by navigating up to the root.
toTree :: Zipper a -> BinTree a
toTree (Zipper currentFocus []) = currentFocus -- If no parents, currentFocus is the root
toTree (Zipper currentFocus (LContext pv rs : ps)) =
    -- Reconstruct the parent node: parentValue, currentFocus as left child, rightSibling as right child
    toTree (Zipper (BT pv (Just currentFocus) rs) ps)
toTree (Zipper currentFocus (RContext pv ls : ps)) =
    -- Reconstruct the parent node: parentValue, leftSibling as left child, currentFocus as right child
    toTree (Zipper (BT pv ls (Just currentFocus)) ps)

-- Returns the value of the node currently in focus.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- Moves the focus to the left child of the current node.
-- Returns Nothing if there is no left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) ps) = Just (Zipper l (LContext v r : ps))
left _ = Nothing -- No left child

-- Moves the focus to the right child of the current node.
-- Returns Nothing if there is no right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) ps) = Just (Zipper r (RContext v l : ps))
right _ = Nothing -- No right child

-- Moves the focus to the parent of the current node.
-- Returns Nothing if the current node is the root (no parent).
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing -- Already at the root
up (Zipper current (LContext pv rs : ps)) =
    -- Reconstruct the parent with 'current' as its left child
    Just (Zipper (BT pv (Just current) rs) ps)
up (Zipper current (RContext pv ls : ps)) =
    -- Reconstruct the parent with 'current' as its right child
    Just (Zipper (BT pv ls (Just current)) ps)

-- Sets the value of the node currently in focus.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) ps) = Zipper (BT x l r) ps

-- Sets the left child of the node currently in focus.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) ps) = Zipper (BT v newLeft r) ps

-- Sets the right child of the node currently in focus.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) ps) = Zipper (BT v l newRight) ps
