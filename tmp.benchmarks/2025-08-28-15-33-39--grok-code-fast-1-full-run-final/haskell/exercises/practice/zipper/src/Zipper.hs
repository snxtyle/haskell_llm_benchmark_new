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

data Zipper a = Zipper { focus :: BinTree a, context :: [Crumb a] } deriving (Eq, Show)

data Crumb a = LeftCrumb a (Maybe (BinTree a)) | RightCrumb a (Maybe (BinTree a)) deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree zipper = case up zipper of
  Nothing -> focus zipper
  Just z -> toTree z

value :: Zipper a -> a
value zipper = btValue (focus zipper)

left :: Zipper a -> Maybe (Zipper a)
left zipper = case btLeft (focus zipper) of
  Nothing -> Nothing
  Just l -> Just $ Zipper l (LeftCrumb (btValue (focus zipper)) (btRight (focus zipper)) : context zipper)

right :: Zipper a -> Maybe (Zipper a)
right zipper = case btRight (focus zipper) of
  Nothing -> Nothing
  Just r -> Just $ Zipper r (RightCrumb (btValue (focus zipper)) (btLeft (focus zipper)) : context zipper)

up :: Zipper a -> Maybe (Zipper a)
up zipper = case context zipper of
  [] -> Nothing
  (LeftCrumb v r : cs) -> Just $ Zipper (BT v (Just (focus zipper)) r) cs
  (RightCrumb v l : cs) -> Just $ Zipper (BT v l (Just (focus zipper))) cs

setValue :: a -> Zipper a -> Zipper a
setValue x zipper = zipper { focus = (focus zipper) { btValue = x } }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree zipper = zipper { focus = (focus zipper) { btLeft = tree } }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree zipper = zipper { focus = (focus zipper) { btRight = tree } }
