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

import Data.Maybe (fromMaybe)

-- | A binary tree.
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- | A trail of bread crumbs from a node back to the root of the tree.
-- Each crumb stores the value of the parent node and the sibling subtree
-- that was not visited.
type Breadcrumbs a = [Crumb a]

-- | A single crumb in the trail.
data Crumb a = LeftCrumb  a (Maybe (BinTree a))  -- Came from the left child. Stores parent value and right sibling.
             | RightCrumb a (Maybe (BinTree a))  -- Came from the right child. Stores parent value and left sibling.
             deriving (Eq, Show)

-- | A binary tree zipper.
-- It consists of the tree that is currently in focus and a trail of
-- bread crumbs back to the root of the original tree.
data Zipper a = Zipper { focus :: BinTree a
                       , path  :: Breadcrumbs a
                       } deriving (Eq, Show)

-- | Get a zipper focussed on the root of the tree.
fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree z = goUp z
  where
    goUp :: Zipper a -> BinTree a
    goUp (Zipper t []) = t
    goUp (Zipper t (LeftCrumb v r : bs))  = goUp (Zipper (BT v (Just t) r) bs)
    goUp (Zipper t (RightCrumb v l : bs)) = goUp (Zipper (BT v l (Just t)) bs)

-- | Get the value of the focus node.
value :: Zipper a -> a
value (Zipper t _) = btValue t

-- | Move the focus to the left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) bs) = fmap (\l' -> Zipper l' (LeftCrumb v r : bs)) l

-- | Move the focus to the right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) bs) = fmap (\r' -> Zipper r' (RightCrumb v l : bs)) r

-- | Move the focus to the parent node.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper t (LeftCrumb v r : bs))  = Just (Zipper (BT v (Just t) r) bs)
up (Zipper t (RightCrumb v l : bs)) = Just (Zipper (BT v l (Just t)) bs)
up (Zipper _ []) = Nothing

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

-- | Set the left subtree of the focus node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) bs) = Zipper (BT v l r) bs

-- | Set the right subtree of the focus node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) bs) = Zipper (BT v l r) bs
