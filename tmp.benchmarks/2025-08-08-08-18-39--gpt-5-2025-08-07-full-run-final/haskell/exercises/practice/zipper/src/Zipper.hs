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

-- Context for reconstructing the tree when moving up
data Context a
  = TopC
  | LeftC  a (Maybe (BinTree a)) (Context a)   -- came from left child; store parent value and right sibling
  | RightC a (Maybe (BinTree a)) (Context a)   -- came from right child; store parent value and left sibling
  deriving (Eq, Show)

-- Zipper keeps the current focus and the context to the root
data Zipper a = Zipper (BinTree a) (Context a)
  deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper tree TopC

toTree :: Zipper a -> BinTree a
toTree (Zipper t TopC) = t
toTree (Zipper t (LeftC v r ctx))  = toTree (Zipper (BT v (Just t) r) ctx)
toTree (Zipper t (RightC v l ctx)) = toTree (Zipper (BT v l (Just t)) ctx)

value :: Zipper a -> a
value (Zipper (BT v _ _) _) = v

left :: Zipper a -> Maybe (Zipper a)
left (Zipper (BT v l r) ctx) =
  case l of
    Nothing  -> Nothing
    Just lt  -> Just (Zipper lt (LeftC v r ctx))

right :: Zipper a -> Maybe (Zipper a)
right (Zipper (BT v l r) ctx) =
  case r of
    Nothing  -> Nothing
    Just rt  -> Just (Zipper rt (RightC v l ctx))

up :: Zipper a -> Maybe (Zipper a)
up (Zipper _ TopC)                 = Nothing
up (Zipper t (LeftC v r ctx))      = Just (Zipper (BT v (Just t) r) ctx)
up (Zipper t (RightC v l ctx))     = Just (Zipper (BT v l (Just t)) ctx)

setValue :: a -> Zipper a -> Zipper a
setValue x (Zipper (BT _ l r) ctx) = Zipper (BT x l r) ctx

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft newLeft (Zipper (BT v _ r) ctx) = Zipper (BT v newLeft r) ctx

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight newRight (Zipper (BT v l _) ctx) = Zipper (BT v l newRight) ctx
