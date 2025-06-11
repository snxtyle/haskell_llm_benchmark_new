module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral n
  | n < 0 = error "Size must be non-negative"
  | n == 0 = []
  | n == 1 = [[1]]
  | otherwise =
    let inner = spiral (n - 2)
        offset = 4 * n - 4
        offsetInner = map (map (+ offset)) inner

        topRow = [1 .. n]
        rightColBody = [n + 1 .. 2 * n - 2]
        bottomRow = reverse [2 * n - 1 .. 3 * n - 2]
        leftColBody = reverse [3 * n - 1 .. 4 * n - 4]

        middleRows = myZipWith3 (\l m r -> [l] ++ m ++ [r]) leftColBody offsetInner rightColBody
    in [topRow] ++ middleRows ++ [bottomRow]
  where
    myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
    myZipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : myZipWith3 f xs ys zs
    myZipWith3 _ _ _ _ = []
