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

data Crumb a = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a))
  deriving (Eq, Show)

data Zipper a = Zipper { focus :: BinTree a, crumbs :: [Crumb a] }
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree zipper = toTree (case up zipper of Just z -> z)

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
up (Zipper tree (LeftCrumb val rightTree:crumbs)) = 
  Just $ Zipper (BT val (Just tree) rightTree) crumbs
up (Zipper tree (RightCrumb val leftTree:crumbs)) = 
  Just $ Zipper (BT val leftTree (Just tree)) crumbs

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper tree crumbs) = Zipper (tree { btValue = x }) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper tree crumbs) = Zipper (tree { btLeft = newLeft }) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper tree crumbs) = Zipper (tree { btRight = newRight }) crumbs
