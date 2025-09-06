module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
  | n < 0 = []
  | n == 0 = [[]]
  | n > length xs = []
  | otherwise = map (map digitToInt) $ take (length xs - n + 1) $ map (take n) (tails xs)
  where
    digitToInt c = fromEnum c - fromEnum '0'
    tails [] = []
    tails ys = ys : tails (drop 1 ys)
