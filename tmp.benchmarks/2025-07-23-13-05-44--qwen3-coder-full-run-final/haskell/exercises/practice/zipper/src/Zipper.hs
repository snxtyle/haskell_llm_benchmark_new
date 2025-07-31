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

data Zipper a = Zipper { focus :: BinTree a
                       , crumbs :: [Crumb a]
                       } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree crumbs) = rebuildRoot tree crumbs
  where
    rebuildRoot t [] = t
    rebuildRoot t (LeftCrumb v r:cs) = rebuildRoot (BT v (Just t) r) cs
    rebuildRoot t (RightCrumb v l:cs) = rebuildRoot (BT v l (Just t)) cs

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v Nothing _r) cs) = Nothing
left (Zipper (BT v (Just l) r) cs) = Just (Zipper l (LeftCrumb v r : cs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v _l Nothing) cs) = Nothing
right (Zipper (BT v l (Just r)) cs) = Just (Zipper r (RightCrumb v l : cs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (LeftCrumb v r : cs)) = Just (Zipper (BT v (Just tree) r) cs)
up (Zipper tree (RightCrumb v l : cs)) = Just (Zipper (BT v l (Just tree)) cs)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) cs) = Zipper (BT x l r) cs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) cs) = Zipper (BT v l r) cs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) cs) = Zipper (BT v l r) cs
