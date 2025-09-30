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

-- Crumb represents one level in the path from root to current focus
-- It stores whether we went left or right, the value at that node,
-- and the other subtree we didn't traverse
data Crumb a = LeftCrumb a (Maybe (BinTree a))   -- went left, store value and right subtree
             | RightCrumb a (Maybe (BinTree a))  -- went right, store value and left subtree
             deriving (Eq, Show)

-- Zipper contains the current focus (subtree) and breadcrumbs (path from root)
data Zipper a = Zipper (BinTree a) [Crumb a]
              deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree crumbs) = go tree crumbs
  where
    go t [] = t
    go t (LeftCrumb val rightTree : cs) = 
      go (BT val (Just t) rightTree) cs
    go t (RightCrumb val leftTree : cs) = 
      go (BT val leftTree (Just t)) cs

value :: Zipper a -> a
value (Zipper tree _) = btValue tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT val Nothing _) _) = Nothing
left (Zipper (BT val (Just leftTree) rightTree) crumbs) = 
  Just (Zipper leftTree (LeftCrumb val rightTree : crumbs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT val _ Nothing) _) = Nothing
right (Zipper (BT val leftTree (Just rightTree)) crumbs) = 
  Just (Zipper rightTree (RightCrumb val leftTree : crumbs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (LeftCrumb val rightTree : crumbs)) = 
  Just (Zipper (BT val (Just tree) rightTree) crumbs)
up (Zipper tree (RightCrumb val leftTree : crumbs)) = 
  Just (Zipper (BT val leftTree (Just tree)) crumbs)

setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper (BT _ leftTree rightTree) crumbs) = 
  Zipper (BT newVal leftTree rightTree) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT val _ rightTree) crumbs) = 
  Zipper (BT val newLeft rightTree) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT val leftTree _) crumbs) = 
  Zipper (BT val leftTree newRight) crumbs
