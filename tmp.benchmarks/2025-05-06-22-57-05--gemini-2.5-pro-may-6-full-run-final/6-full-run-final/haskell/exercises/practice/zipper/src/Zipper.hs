module Zipper
 ( BinTree(BT)
 , Zipper -- Added Zipper type to exports
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

-- Breadcrumbs store the path taken from the root.
-- For a step from a parent to a child, we store:
--   - The parent's value.
--   - The sibling tree that was not part of the path.
data Crumb a = LeftCrumb a (Maybe (BinTree a))  -- Parent's value, Parent's right child
             | RightCrumb a (Maybe (BinTree a)) -- Parent's value, Parent's left child
             deriving (Eq, Show)

-- A zipper stores the focused subtree and the list of crumbs (path from root).
data Zipper a = Zipper (BinTree a) [Crumb a]
              deriving (Eq, Show)

-- Creates a zipper from a binary tree, with focus on the root.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- Reconstructs the full binary tree from a zipper.
-- It navigates up to the root, rebuilding the tree along the way.
toTree :: Zipper a -> BinTree a
toTree (Zipper focus []) = focus -- If no crumbs, focus is the root
toTree (Zipper focus (c:cs)) = toTree parentZipper
  where
    parentNode = case c of
      LeftCrumb val r -> BT val (Just focus) r  -- Focus was left child
      RightCrumb val l -> BT val l (Just focus) -- Focus was right child
    parentZipper = Zipper parentNode cs

-- Gets the value of the currently focused node.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- Moves focus to the left child of the current node.
-- Returns Nothing if there is no left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT pVal (Just lNode) pRight) crumbs) =
    Just (Zipper lNode (LeftCrumb pVal pRight : crumbs))
left (Zipper (BT _ Nothing _) _) = Nothing

-- Moves focus to the right child of the current node.
-- Returns Nothing if there is no right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT pVal pLeft (Just rNode)) crumbs) =
    Just (Zipper rNode (RightCrumb pVal pLeft : crumbs))
right (Zipper (BT _ _ Nothing) _) = Nothing

-- Moves focus to the parent of the current node.
-- Returns Nothing if the current node is the root.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing -- Already at the root, no parent
up (Zipper focusNode (c:cs)) =
    case c of
      LeftCrumb parentVal parentRightSibling ->
          Just (Zipper (BT parentVal (Just focusNode) parentRightSibling) cs)
      RightCrumb parentVal parentLeftSibling ->
          Just (Zipper (BT parentVal parentLeftSibling (Just focusNode)) cs)

-- Updates the value of the focused node.
setValue :: a -> Zipper a -> Zipper a
setValue newValue (Zipper (BT _ l r) crumbs) =
    Zipper (BT newValue l r) crumbs

-- Replaces the left child of the focused node.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT val _ r) crumbs) =
    Zipper (BT val newLeft r) crumbs

-- Replaces the right child of the focused node.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT val l _) crumbs) =
    Zipper (BT val l newRight) crumbs
