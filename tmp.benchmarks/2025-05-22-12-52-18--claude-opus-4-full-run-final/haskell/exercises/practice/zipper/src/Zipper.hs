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

-- A zipper consists of the current focus tree and a list of contexts
-- Each context stores: parent value, direction we came from, and the sibling tree
data Direction = L | R deriving (Eq, Show)
data Context a = Context a Direction (Maybe (BinTree a)) deriving (Eq, Show)
data Zipper a = Zipper (BinTree a) [Context a] deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper tree []) = tree
toTree zipper = case up zipper of
  Nothing -> error "Invalid zipper state"
  Just parent -> toTree parent

value :: Zipper a -> a
value (Zipper tree _) = btValue tree

left :: Zipper a -> Maybe (Zipper a)
left (Zipper tree contexts) = case btLeft tree of
  Nothing -> Nothing
  Just leftTree -> Just $ Zipper leftTree (Context (btValue tree) L (btRight tree) : contexts)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper tree contexts) = case btRight tree of
  Nothing -> Nothing
  Just rightTree -> Just $ Zipper rightTree (Context (btValue tree) R (btLeft tree) : contexts)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper tree (Context parentValue dir sibling : rest)) = 
  Just $ Zipper parentTree rest
  where
    parentTree = case dir of
      L -> BT parentValue (Just tree) sibling
      R -> BT parentValue sibling (Just tree)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper tree contexts) = Zipper (tree { btValue = x }) contexts

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper tree contexts) = Zipper (tree { btLeft = newLeft }) contexts

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper tree contexts) = Zipper (tree { btRight = newRight }) contexts
