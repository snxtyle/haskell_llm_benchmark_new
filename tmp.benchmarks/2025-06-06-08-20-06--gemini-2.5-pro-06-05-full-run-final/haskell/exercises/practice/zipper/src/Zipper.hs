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

-- A breadcrumb stores the context of a parent node.
-- It records which direction we came from (left or right),
-- the parent's value, and the sibling subtree.
data Crumb a = GoLeft a (Maybe (BinTree a))
             | GoRight a (Maybe (BinTree a))
             deriving (Eq, Show)

-- A list of breadcrumbs represents the path from the root to the parent of the current focus.
type Breadcrumbs a = [Crumb a]

-- A zipper represents a focused location within a BinTree.
-- It contains the focused subtree and the path (breadcrumbs) back to the root.
data Zipper a = Zipper (BinTree a) (Breadcrumbs a) deriving (Eq, Show)

-- fromTree creates a zipper from a tree, with the focus at the root.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- toTree recovers the full tree from a zipper by navigating up to the root.
toTree :: Zipper a -> BinTree a
toTree z =
    case up z of
        Nothing -> let (Zipper t _) = z in t
        Just p  -> toTree p

-- value returns the value of the node at the focus.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- left moves the focus to the left child. Returns Nothing if there is no left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) bs) = Just (Zipper l (GoLeft v r : bs))
left (Zipper (BT _ Nothing _) _)   = Nothing

-- right moves the focus to the right child. Returns Nothing if there is no right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) bs) = Just (Zipper r (GoRight v l : bs))
right (Zipper (BT _ _ Nothing) _)   = Nothing

-- up moves the focus to the parent. Returns Nothing if we are at the root.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (c:cs)) =
    Just $ case c of
        GoLeft p_val p_right  -> Zipper (BT p_val (Just t) p_right) cs
        GoRight p_val p_left -> Zipper (BT p_val p_left (Just t)) cs

-- setValue changes the value of the focused node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

-- setLeft changes the left child of the focused node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft new_l (Zipper (BT v _ r) bs) = Zipper (BT v new_l r) bs

-- setRight changes the right child of the focused node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight new_r (Zipper (BT v l _) bs) = Zipper (BT v l new_r) bs
