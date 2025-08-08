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

-- Breadcrumbs to reconstruct the path back to the root.
data Crumb a
  = LeftCrumb  a (Maybe (BinTree a))   -- came from left child, keep parent value and right subtree
  | RightCrumb a (Maybe (BinTree a))   -- came from right child, keep parent value and left subtree
  deriving (Eq, Show)

-- Zipper holds the current focus and the path to the root.
data Zipper a = Zipper { zFocus  :: BinTree a
                       , zCrumbs :: [Crumb a]
                       } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree = rebuild
  where
    rebuild (Zipper t []) = t
    rebuild (Zipper t (LeftCrumb v r : cs)) =
      rebuild (Zipper (BT v (Just t) r) cs)
    rebuild (Zipper t (RightCrumb v l : cs)) =
      rebuild (Zipper (BT v l (Just t)) cs)

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) cs) =
  case l of
    Nothing     -> Nothing
    Just ltree  -> Just (Zipper ltree (LeftCrumb v r : cs))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) cs) =
  case r of
    Nothing     -> Nothing
    Just rtree  -> Just (Zipper rtree (RightCrumb v l : cs))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper t (LeftCrumb v r : cs))  = Just (Zipper (BT v (Just t) r) cs)
up (Zipper t (RightCrumb v l : cs)) = Just (Zipper (BT v l (Just t)) cs)
up (Zipper _ [])                    = Nothing

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) cs) = Zipper (BT x l r) cs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree (Zipper (BT v _ r) cs) = Zipper (BT v tree r) cs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree (Zipper (BT v l _) cs) = Zipper (BT v l tree) cs
