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

-- | A simple binary tree.
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- | A crumb remembers the information needed to reconstruct the
-- parent node when moving back up the tree.
data Crumb a = LeftCrumb  a (Maybe (BinTree a))  -- ^ came from left  child, stores parent value and right  subtree
             | RightCrumb a (Maybe (BinTree a))  -- ^ came from right child, stores parent value and left   subtree
             deriving (Eq, Show)

-- | The zipper focuses on one subtree and carries breadcrumbs that
-- allow navigating back to the root.
data Zipper a = Zipper (BinTree a) [Crumb a]
              deriving (Eq, Show)

-- | Get a zipper out of a binary tree.  The focus is on the root node.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- | Convert a zipper back into a (complete) binary tree.
toTree :: Zipper a -> BinTree a
toTree (Zipper t bs) = rebuild t bs
  where
    rebuild :: BinTree a -> [Crumb a] -> BinTree a
    rebuild t []                        = t
    rebuild t (LeftCrumb v r  : cs) = rebuild (BT v (Just t) r) cs
    rebuild t (RightCrumb v l : cs) = rebuild (BT v l (Just t)) cs

-- | Value of the focus node.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- | Move focus to the left child, if it exists.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) bs) = Just (Zipper l (LeftCrumb v r : bs))
left _                             = Nothing

-- | Move focus to the right child, if it exists.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) bs) = Just (Zipper r (RightCrumb v l : bs))
right _                             = Nothing

-- | Move focus to the parent, if it exists.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _       [])                        = Nothing
up (Zipper t (LeftCrumb  v r : bs)) = Just $ Zipper (BT v (Just t) r) bs
up (Zipper t (RightCrumb v l : bs)) = Just $ Zipper (BT v l (Just t)) bs

-- | Replace the value at the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) bs) = Zipper (BT x l r) bs

-- | Replace the left child subtree at the focus node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) bs) = Zipper (BT v newLeft r) bs

-- | Replace the right child subtree at the focus node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) bs) = Zipper (BT v l newRight) bs
