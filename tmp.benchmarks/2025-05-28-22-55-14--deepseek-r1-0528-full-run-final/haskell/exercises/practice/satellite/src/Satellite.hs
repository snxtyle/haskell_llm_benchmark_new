module Satellite (treeFromTraversals) where

import BinaryTree (BinaryTree(..))
import Data.List (elemIndex)

treeFromTraversals :: Ord a => [a] -> [a] -> Maybe (BinaryTree a)
treeFromTraversals pre inord
  | length pre /= length inord = Nothing
  | null pre = Just Leaf
  | otherwise =
      case elemIndex (head pre) inord of
        Nothing -> Nothing
        Just idx ->
          let inLeft = take idx inord
              inRight = drop (idx + 1) inord
              preLeft = take idx (tail pre)
              preRight = drop idx (tail pre)
          in (\left right -> Branch left (head pre) right) 
             <$> treeFromTraversals preLeft inLeft 
             <*> treeFromTraversals preRight inRight
