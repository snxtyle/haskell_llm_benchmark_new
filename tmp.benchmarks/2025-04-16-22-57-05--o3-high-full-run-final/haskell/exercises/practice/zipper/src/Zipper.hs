module Zipper
 ( BinTree(BT)
 , Zipper            -- export the type, but not its constructors
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

-- | A simple immutable binary tree.
data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } deriving (Eq, Show)

-- | The breadcrumbs we leave while walking down the tree.
--   Each crumb remembers:
--     • the value stored in the parent node
--     • the sibling subtree we did NOT walk into
data Crumb a
      = LeftCrumb  a (Maybe (BinTree a))  -- came from the left  child, remember right sibling
      | RightCrumb a (Maybe (BinTree a))  -- came from the right child, remember left  sibling
      deriving (Eq, Show)

-- | The zipper consists of the currently focused subtree
--   together with the breadcrumbs back to the root.
data Zipper a = Zipper
    { focus       :: BinTree a      -- current subtree in focus
    , breadcrumbs :: [Crumb a]      -- path back towards the root (nearest first)
    } deriving (Eq, Show)

-- | Create a zipper focused on the root of the supplied tree.
fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

-- | Reconstruct the full tree represented by this zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper t bs) = rebuild t bs
  where
    rebuild :: BinTree a -> [Crumb a] -> BinTree a
    rebuild curr []                         = curr
    rebuild curr (LeftCrumb  v r  : rest)   = rebuild (BT v (Just curr) r) rest
    rebuild curr (RightCrumb v l  : rest)   = rebuild (BT v l (Just curr)) rest

-- | Return the value stored at the focus.
value :: Zipper a -> a
value (Zipper t _) = btValue t

-- | Move the focus to the left child, if it exists.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper t bs) =
  case btLeft t of
    Nothing   -> Nothing
    Just lSub -> Just $ Zipper lSub (LeftCrumb (btValue t) (btRight t) : bs)

-- | Move the focus to the right child, if it exists.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper t bs) =
  case btRight t of
    Nothing   -> Nothing
    Just rSub -> Just $ Zipper rSub (RightCrumb (btValue t) (btLeft t) : bs)

-- | Move the focus to the parent node, if it exists.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper _       [])                = Nothing               -- already at root
up (Zipper curr (c : bs)) =
  case c of
    LeftCrumb  v r -> Just $ Zipper (BT v (Just curr) r) bs
    RightCrumb v l -> Just $ Zipper (BT v l (Just curr)) bs

-- | Replace the value at the focus.
setValue :: a -> Zipper a -> Zipper a
setValue newVal (Zipper t bs) = Zipper t{ btValue = newVal } bs

-- | Replace the left subtree of the focus.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper t bs) = Zipper t{ btLeft = newLeft } bs

-- | Replace the right subtree of the focus.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper t bs) = Zipper t{ btRight = newRight } bs
