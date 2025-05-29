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

data Crumb a = LeftCrumb a (Maybe (BinTree a))
             | RightCrumb a (Maybe (BinTree a))
             deriving (Eq, Show)

data Zipper a = Zipper { focus :: BinTree a, context :: [Crumb a] } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree []

toTree :: Zipper a -> BinTree a
toTree (Zipper t []) = t
toTree z = toTree (fromJust (up z))

value :: Zipper a -> a
value = btValue . focus

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) ctx) = case l of
    Nothing -> Nothing
    Just lt -> Just $ Zipper lt (LeftCrumb v r : ctx)

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) ctx) = case r of
    Nothing -> Nothing
    Just rt -> Just $ Zipper rt (RightCrumb v l : ctx)

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ []) = Nothing
up (Zipper t (LeftCrumb v r:ctx)) = Just $ Zipper (BT v (Just t) r) ctx
up (Zipper t (RightCrumb v l:ctx)) = Just $ Zipper (BT v l (Just t)) ctx

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) ctx) = Zipper (BT x l r) ctx

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) ctx) = Zipper (BT v newLeft r) ctx

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) ctx) = Zipper (BT v l newRight) ctx

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust called on Nothing"
