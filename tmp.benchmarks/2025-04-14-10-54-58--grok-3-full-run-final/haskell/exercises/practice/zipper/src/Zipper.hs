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

-- Breadcrumb represents the path we took to get to the current focus
data Breadcrumb a = LeftCrumb a (Maybe (BinTree a))  -- we went left, store parent value and right subtree
                  | RightCrumb a (Maybe (BinTree a)) -- we went right, store parent value and left subtree
                  deriving (Eq, Show)

-- Zipper is the current focus (tree) and the path back to root
data Zipper a = Zipper (BinTree a) [Breadcrumb a]
              deriving (Eq, Show)

-- Convert a tree to a zipper, focusing on the root
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Convert a zipper back to a tree
toTree :: Zipper a -> BinTree a
toTree zipper = goToRoot zipper
  where
    goToRoot z@(Zipper tree crumbs) =
      case crumbs of
        [] -> tree
        _  -> case up z of
                Just parentZipper -> goToRoot parentZipper
                Nothing -> tree  -- This case shouldn't happen if breadcrumbs exist

-- Get the value at the current focus
value :: Zipper a -> a
value (Zipper tree _) = btValue tree

-- Move focus to the left child if it exists
left :: Zipper a -> Maybe (Zipper a)
left (Zipper tree crumbs) = case btLeft tree of
    Nothing -> Nothing
    Just leftTree -> Just $ Zipper leftTree (LeftCrumb (btValue tree) (btRight tree) : crumbs)

-- Move focus to the right child if it exists
right :: Zipper a -> Maybe (Zipper a)
right (Zipper tree crumbs) = case btRight tree of
    Nothing -> Nothing
    Just rightTree -> Just $ Zipper rightTree (RightCrumb (btValue tree) (btLeft tree) : crumbs)

-- Move focus up to the parent if it exists
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (LeftCrumb parentVal rightSibling : crumbs)) =
    Just $ Zipper (BT parentVal (Just tree) rightSibling) crumbs
up (Zipper tree (RightCrumb parentVal leftSibling : crumbs)) =
    Just $ Zipper (BT parentVal leftSibling (Just tree)) crumbs

-- Set the value at the current focus
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper tree crumbs) = Zipper (tree { btValue = x }) crumbs

-- Set the left subtree at the current focus
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper tree crumbs) = Zipper (tree { btLeft = newLeft }) crumbs

-- Set the right subtree at the current focus
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper tree crumbs) = Zipper (tree { btRight = newRight }) crumbs
