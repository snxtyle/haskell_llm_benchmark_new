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

-- A breadcrumb keeps the context needed to reconstruct the parent node
-- when moving back up: the parent's value and the sibling subtree that
-- is not currently focused.
data Crumb a
  = LeftCrumb  a (Maybe (BinTree a))   -- came from left; holds parent value and right subtree
  | RightCrumb a (Maybe (BinTree a))   -- came from right; holds parent value and left subtree
  deriving (Eq, Show)

-- The zipper keeps the current focus and the path to the root.
data Zipper a = Zipper (BinTree a) [Crumb a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z             = case up z of
  Just z' -> toTree z'
  Nothing -> case z of
    Zipper t _ -> t

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) bs) =
  case l of
    Nothing   -> Nothing
    Just lt   -> Just $ Zipper lt (LeftCrumb v r : bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) bs) =
  case r of
    Nothing   -> Nothing
    Just rt   -> Just $ Zipper rt (RightCrumb v l : bs)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper cur (c:cs)) =
  case c of
    LeftCrumb pv pr  -> Just $ Zipper (BT pv (Just cur) pr) cs
    RightCrumb pv pl -> Just $ Zipper (BT pv pl (Just cur)) cs
up (Zipper _ []) = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l' (Zipper (BT v _ r) bs) = Zipper (BT v l' r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r' (Zipper (BT v l _) bs) = Zipper (BT v l r') bs
