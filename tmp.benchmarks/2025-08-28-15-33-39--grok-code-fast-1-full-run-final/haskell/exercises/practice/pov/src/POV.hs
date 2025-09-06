module POV (fromPOV, tracePathBetween) where

import Data.Tree (Tree(..))
import Data.List (partition)
import Data.Maybe (mapMaybe)

findPath :: Eq a => a -> Tree a -> Maybe [a]
findPath target (Node root children) = if root == target then Just [root]
                                       else case mapMaybe (findPath target) children of
                                              (p:_) -> Just (root : p)
                                              [] -> Nothing

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix _ [] = []
commonPrefix (x:xs) (y:ys) | x == y = x : commonPrefix xs ys
                           | otherwise = []

addChildTo :: Eq a => a -> Tree a -> Tree a -> Tree a
addChildTo label newChild (Node r cs) = if r == label then Node r (cs ++ [newChild])
                                        else Node r (map (addChildTo label newChild) cs)

fromPOV :: Eq a => a -> Tree a -> Maybe (Tree a)
fromPOV x tree = do
  path <- findPath x tree
  return $ reorient path tree

reorient :: Eq a => [a] -> Tree a -> Tree a
reorient path tree = go path tree
  where go [y] (Node r cs) | r == y = Node r cs
                           | otherwise = error "root mismatch"
        go (p:ps) (Node r cs) | r == p = 
          let next = case ps of (n:_) -> n
              (subs, others) = partition (\(Node r' _) -> r' == next) cs
              sub = case subs of [s] -> s
              reoriented = go ps sub
          in addChildTo next (Node r others) reoriented
                              | otherwise = error "root mismatch"
        go [] _ = error "empty path"
        go _ _ = error "unexpected pattern"

tracePathBetween :: Eq a => a -> a -> Tree a -> Maybe [a]
tracePathBetween from to tree = do
  pFrom <- findPath from tree
  pTo <- findPath to tree
  let pref = commonPrefix pFrom pTo
      len = length pref
      up = reverse (drop len pFrom)
      down = drop len pTo
      lca = if null pref then [] else [last pref]
  return $ up ++ lca ++ down
