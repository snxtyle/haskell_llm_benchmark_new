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

data BinTree a = BT
  { btValue :: a
  , btLeft  :: Maybe (BinTree a)
  , btRight :: Maybe (BinTree a)
  } deriving (Eq, Show)

-- A crumb remembers which side we came from, the parent value,
-- and the sibling subtree that we didn't descend into.
data Crumb a
  = LeftCrumb  a (Maybe (BinTree a))   -- came down into left child, store parent value and right subtree
  | RightCrumb a (Maybe (BinTree a))   -- came down into right child, store parent value and left subtree
  deriving (Eq, Show)

-- Zipper holds the current focus plus the path (breadcrumbs) back to the root
data Zipper a = Zipper (BinTree a) [Crumb a]
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

toTree :: Zipper a -> BinTree a
toTree (Zipper t [])     = t
toTree z                 = case up z of
                             Just z' -> toTree z'
                             Nothing -> let (Zipper t' _) = z in t'

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT _ Nothing _) _) = Nothing
left (Zipper (BT v (Just l) r) bs) =
  Just $ Zipper l (LeftCrumb v r : bs)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT _ _ Nothing) _) = Nothing
right (Zipper (BT v l (Just r)) bs) =
  Just $ Zipper r (RightCrumb v l : bs)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _    []                     ) = Nothing
up (Zipper t (LeftCrumb  v r : bs)) =
  let parent = BT v (Just t) r
  in Just $ Zipper parent bs
up (Zipper t (RightCrumb v l : bs)) =
  let parent = BT v l (Just t)
  in Just $ Zipper parent bs

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newL (Zipper (BT v _ r) bs) = Zipper (BT v newL r) bs

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newR (Zipper (BT v l _) bs) = Zipper (BT v l newR) bs
