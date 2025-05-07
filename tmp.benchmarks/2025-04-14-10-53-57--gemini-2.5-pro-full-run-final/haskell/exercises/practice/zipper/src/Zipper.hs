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

-- | A binary tree.
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- | Represents the path from the root to the parent of the focus.
-- Stores the parent's value, the sibling tree, and the direction taken from the parent.
data Crumb a = LeftCrumb a (Maybe (BinTree a)) -- Came from parent's left, stores parent value and right sibling
             | RightCrumb a (Maybe (BinTree a)) -- Came from parent's right, stores parent value and left sibling
             deriving (Eq, Show)

-- | The context is a list of crumbs representing the path upwards.
type Context a = [Crumb a]

-- | A zipper for a binary tree. It contains the focused node and the context.
data Zipper a = Zipper { focus :: BinTree a
                       , context :: Context a
                       } deriving (Eq, Show)

-- | Creates a zipper focused on the root of the tree.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- | Converts a zipper back to the full tree it represents.
-- Navigates up to the root regardless of the current focus.
toTree :: Zipper a -> BinTree a
toTree z = case up z of
             Nothing -> focus z -- Already at the root
             Just parentZipper -> toTree parentZipper

-- | Gets the value of the node currently in focus.
value :: Zipper a -> a
value (Zipper f _) = btValue f

-- | Moves the focus to the left child. Returns Nothing if there is no left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v (Just l) r) c) = Just $ Zipper l (LeftCrumb v r : c)
left _                             = Nothing -- No left child

-- | Moves the focus to the right child. Returns Nothing if there is no right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l (Just r)) c) = Just $ Zipper r (RightCrumb v l : c)
right _                             = Nothing -- No right child

-- | Moves the focus to the parent node. Returns Nothing if the focus is already at the root.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper f (LeftCrumb v r : cs))  = Just $ Zipper (BT v (Just f) r) cs -- Came from left, reconstruct parent
up (Zipper f (RightCrumb v l : cs)) = Just $ Zipper (BT v l (Just f)) cs -- Came from right, reconstruct parent
up _                                = Nothing -- Already at the root (empty context)

-- | Sets the value of the focused node.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) c) = Zipper (BT x l r) c

-- | Replaces the left child of the focused node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree (Zipper (BT v _ r) c) = Zipper (BT v tree r) c

-- | Replaces the right child of the focused node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree (Zipper (BT v l _) c) = Zipper (BT v l tree) c
