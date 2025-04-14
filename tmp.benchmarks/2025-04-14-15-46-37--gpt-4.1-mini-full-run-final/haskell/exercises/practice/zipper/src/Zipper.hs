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

-- A Crumb represents the context needed to go back up the tree.
-- It stores the value of the parent node and the sibling subtree on the other side.
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- came from left child, store parent's value and right subtree
             | RightCrumb a (Maybe (BinTree a)) -- came from right child, store parent's value and left subtree
             deriving (Eq, Show)

-- The Zipper is a focus on a subtree, plus a list of crumbs representing the path back to the root.
data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = case up z of
  Just parentZipper -> toTree parentZipper
  Nothing -> error "Invalid zipper state: no parent but crumbs exist"

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT _ Nothing _) _) = Nothing
left (Zipper (BT v (Just l) r) crumbs) =
  Just $ Zipper l (LeftCrumb v r : crumbs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT _ _ Nothing) _) = Nothing
right (Zipper (BT v l (Just r)) crumbs) =
  Just $ Zipper r (RightCrumb v l : crumbs)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb pv r : crumbs)) =
  Just $ Zipper (BT pv (Just t) r) crumbs
up (Zipper t (RightCrumb pv l : crumbs)) =
  Just $ Zipper (BT pv l (Just t)) crumbs

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) crumbs) = Zipper (BT x l r) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) crumbs) = Zipper (BT v newLeft r) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) crumbs) = Zipper (BT v l newRight) crumbs
