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

-- | Binary tree where every node carries a value and optional left / right
--   children.
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- | A single step in the path back to the root.  Each crumb remembers
--   the value stored in the parent node and the sibling subtree that we
--   did *not* descend into.
data Crumb a
  = LeftCrumb  a (Maybe (BinTree a))      -- ^ we came from the left  child, remembers parent value and right  subtree
  | RightCrumb a (Maybe (BinTree a))      -- ^ we came from the right child, remembers parent value and left   subtree
  deriving (Eq, Show)

type Breadcrumbs a = [Crumb a]

-- | A zipper focuses on a particular subtree while remembering the path
--   back to the root.
data Zipper a = Zipper (BinTree a) (Breadcrumbs a)
  deriving (Eq, Show)

---------------------------------------------------------------------------
-- Construction / deconstruction
---------------------------------------------------------------------------

-- | Create a zipper focused on the root of the supplied tree.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- | Reconstruct the full tree contained in the zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper t crumbs) = foldl rebuild t crumbs
  where
    rebuild acc (LeftCrumb  pv r) = BT pv (Just acc) r
    rebuild acc (RightCrumb pv l) = BT pv l (Just acc)

---------------------------------------------------------------------------
-- Navigation
---------------------------------------------------------------------------

-- | Extract the value at the current focus.
value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

-- | Move focus to the left child.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT _ Nothing  _) _        ) = Nothing
left (Zipper (BT pv (Just l) r) crumbs  ) =
  Just (Zipper l (LeftCrumb pv r : crumbs))

-- | Move focus to the right child.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT _ _ Nothing ) _        ) = Nothing
right (Zipper (BT pv l (Just r)) crumbs  ) =
  Just (Zipper r (RightCrumb pv l : crumbs))

-- | Move focus to the parent node.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _      []              ) = Nothing
up (Zipper focus (c:cs)           ) =
  case c of
    LeftCrumb  pv r -> Just (Zipper (BT pv (Just focus) r) cs)
    RightCrumb pv l -> Just (Zipper (BT pv l (Just focus)) cs)

---------------------------------------------------------------------------
-- Modification
---------------------------------------------------------------------------

-- | Replace the value at the focus.
setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) crumbs) = Zipper (BT x l r) crumbs

-- | Replace the left subtree at the focus.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l' (Zipper (BT v _ r) crumbs) = Zipper (BT v l' r) crumbs

-- | Replace the right subtree at the focus.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r' (Zipper (BT v l _) crumbs) = Zipper (BT v l r') crumbs
