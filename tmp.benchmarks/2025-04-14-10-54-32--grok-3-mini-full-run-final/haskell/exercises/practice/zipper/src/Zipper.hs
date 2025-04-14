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

data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- Parent value and right subtree
             | RightCrumb a (Maybe (BinTree a))  -- Parent value and left subtree
             deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Crumb a]  -- Current tree and trail of crumbs
              deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree  -- At the root, return the tree
toTree zipper = case up zipper of
    Just newZipper -> toTree newZipper  -- Recur until at the root
    Nothing -> error "Invariant violated: Should reach the root"

value :: Zipper a -> a
value (Zipper tree _) = btValue tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT val leftSub rightSub) trail) = case leftSub of
    Just leftTree -> Just (Zipper leftTree (LeftCrumb val rightSub : trail))
    Nothing -> Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT val leftSub rightSub) trail) = case rightSub of
    Just rightTree -> Just (Zipper rightTree (RightCrumb val leftSub : trail))
    Nothing -> Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper tree (crumb:restTrail)) = case crumb of
    LeftCrumb parentVal parentRightSub -> Just (Zipper (BT parentVal (Just tree) parentRightSub) restTrail)
    RightCrumb parentVal parentLeftSub -> Just (Zipper (BT parentVal parentLeftSub (Just tree)) restTrail)
up (Zipper _ []) = Nothing  -- At the top

setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper (BT _ leftSub rightSub) trail) = Zipper (BT newVal leftSub rightSub) trail

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeftSub (Zipper (BT val _ rightSub) trail) = Zipper (BT val newLeftSub rightSub) trail

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRightSub (Zipper (BT val leftSub _) trail) = Zipper (BT val leftSub newRightSub) trail
