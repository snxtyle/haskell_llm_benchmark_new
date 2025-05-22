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

data Crumb a = LeftCrumb a (Maybe (BinTree a))
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

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
left (Zipper (BT val leftTree rightTree) crumbs) =
  case leftTree of
    Nothing -> Nothing
    Just lt -> Just (Zipper lt (LeftCrumb val rightTree : crumbs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT val leftTree rightTree) crumbs) =
  case rightTree of
    Nothing -> Nothing
    Just rt -> Just (Zipper rt (RightCrumb val leftTree : crumbs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper tree crumbs) =
  case crumbs of
    [] -> Nothing
    (LeftCrumb val rightTree : cs) -> 
      Just (Zipper (BT val (Just tree) rightTree) cs)
    (RightCrumb val leftTree : cs) -> 
      Just (Zipper (BT val leftTree (Just tree)) cs)

setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper (BT _ leftTree rightTree) crumbs) =
  Zipper (BT newVal leftTree rightTree) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT val _ rightTree) crumbs) =
  Zipper (BT val newLeft rightTree) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT val leftTree _) crumbs) =
  Zipper (BT val leftTree newRight) crumbs
