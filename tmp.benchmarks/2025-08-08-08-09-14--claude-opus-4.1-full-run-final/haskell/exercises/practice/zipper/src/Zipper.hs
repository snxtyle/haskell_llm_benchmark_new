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

-- Breadcrumb stores the parent's value and the branch we didn't take
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- We went left, store parent value and right subtree
             | RightCrumb a (Maybe (BinTree a)) -- We went right, store parent value and left subtree
             deriving (Eq, Show)

-- Zipper stores the current focus tree and the breadcrumbs (path from root)
data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree zipper = case up zipper of
  Nothing -> error "Invalid zipper state"
  Just parent -> toTree parent

value :: Zipper a -> a
value (Zipper tree _) = btValue tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper tree crumbs) = case btLeft tree of
  Nothing -> Nothing
  Just leftTree -> Just $ Zipper leftTree (LeftCrumb (btValue tree) (btRight tree) : crumbs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper tree crumbs) = case btRight tree of
  Nothing -> Nothing
  Just rightTree -> Just $ Zipper rightTree (RightCrumb (btValue tree) (btLeft tree) : crumbs)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (LeftCrumb parentVal rightTree : rest)) = 
  Just $ Zipper (BT parentVal (Just tree) rightTree) rest
up (Zipper tree (RightCrumb parentVal leftTree : rest)) = 
  Just $ Zipper (BT parentVal leftTree (Just tree)) rest

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper tree crumbs) = Zipper (tree { btValue = x }) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper tree crumbs) = Zipper (tree { btLeft = newLeft }) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper tree crumbs) = Zipper (tree { btRight = newRight }) crumbs
