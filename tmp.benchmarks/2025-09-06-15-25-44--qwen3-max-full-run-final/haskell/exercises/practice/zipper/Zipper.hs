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

-- A Zipper consists of the current focus and a trail (breadcrumbs) to navigate back up
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- Value and right subtree of parent when we went left
             | RightCrumb a (Maybe (BinTree a)) -- Value and left subtree of parent when we went right
             deriving (Eq, Show)

type Trail a = [Crumb a]

data Zipper a = Zipper (BinTree a) (Trail a) deriving (Eq, Show)

-- Create a zipper from a tree, focus on the root
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Get the tree back from the zipper (go to root first)
toTree :: Zipper a -> BinTree a
toTree zipper = case up zipper of
    Nothing -> getCurrentTree zipper
    Just parent -> toTree parent

-- Helper to get the current tree at focus
getCurrentTree :: Zipper a -> BinTree a
getCurrentTree (Zipper tree _) = tree

-- Get the value of the focus node
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- Move focus to the left child, if it exists
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just leftTree) rightTree) trail) = 
    Just $ Zipper leftTree (LeftCrumb v rightTree : trail)
left (Zipper (BT _ Nothing _) _) = Nothing

-- Move focus to the right child, if it exists
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v leftTree (Just rightTree)) trail) = 
    Just $ Zipper rightTree (RightCrumb v leftTree : trail)
right (Zipper (BT _ _ Nothing) _) = Nothing

-- Move focus to the parent, if it exists
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper currentTree (LeftCrumb parentValue parentRight : rest)) = 
    Just $ Zipper (BT parentValue (Just currentTree) parentRight) rest
up (Zipper currentTree (RightCrumb parentValue parentLeft : rest)) = 
    Just $ Zipper (BT parentValue parentLeft (Just currentTree)) rest

-- Set the value of the focus node
setValue :: a -> Zipper a -> Zipper a
setValue newValue (Zipper (BT _ leftTree rightTree) trail) = 
    Zipper (BT newValue leftTree rightTree) trail

-- Set the left child of the focus node
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ rightTree) trail) = 
    Zipper (BT v newLeft rightTree) trail

-- Set the right child of the focus node
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v leftTree _) trail) = 
    Zipper (BT v leftTree newRight) trail
