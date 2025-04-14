module Trinary (readTri, showTri) where

readTri :: String -> Int
readTri = go 0
  where
    go acc [] = acc
    go acc (c:cs) =
      case c of
        '0' -> go (acc * 3) cs
        '1' -> go (acc * 3 + 1) cs
        '2' -> go (acc * 3 + 2) cs
        _   -> 0

showTri :: Int -> String
showTri n
  | n < 0     = error "Negative numbers not supported"
  | n == 0    = "0"
  | otherwise = reverse (go n)
  where
    go 0 = []
    go x = let (q, r) = x `quotRem` 3
               c = case r of
                     0 -> '0'
                     1 -> '1'
                     2 -> '2'
                     _ -> error "Impossible remainder"
           in c : go q
