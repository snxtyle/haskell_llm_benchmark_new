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

-- The Crumb type keeps track of the path taken to reach the current focus.
data Crumb a = LeftCrumb a (Maybe (BinTree a))
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

-- The Zipper contains the current focus and the path (breadcrumbs) to reach it.
data Zipper a = Zipper (BinTree a) [Crumb a]
              deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = toTree' z
  where
    toTree' (Zipper t (LeftCrumb v r:cs)) =
      toTree' (Zipper (BT v (Just t) r) cs)
    toTree' (Zipper t (RightCrumb v l:cs)) =
      toTree' (Zipper (BT v l (Just t)) cs)
    toTree' (Zipper t []) = t

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) crumbs) =
  Just (Zipper l (LeftCrumb v r : crumbs))
left _ = Nothing

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) crumbs) =
  Just (Zipper r (RightCrumb v l : crumbs))
right _ = Nothing

up :: Zipper a -> Maybe (Zipper a)
up (Zipper t (LeftCrumb v r : cs)) =
  Just (Zipper (BT v (Just t) r) cs)
up (Zipper t (RightCrumb v l : cs)) =
  Just (Zipper (BT v l (Just t)) cs)
up (Zipper _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) crumbs) = Zipper (BT x l r) crumbs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) crumbs) = Zipper (BT v l r) crumbs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) crumbs) = Zipper (BT v l r) crumbs
