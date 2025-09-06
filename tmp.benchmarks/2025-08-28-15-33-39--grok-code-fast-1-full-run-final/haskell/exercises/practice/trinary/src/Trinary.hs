module Trinary (readTri, showTri) where

readTri :: String -> Int
readTri s = if all (`elem` "012") s then sum $ zipWith (*) digits powers else 0
  where digits = map (\c -> fromEnum c - fromEnum '0') (reverse s)
        powers = iterate (*3) 1

showTri :: Int -> String
showTri n
  | n == 0    = "0"
  | n > 0     = go n []
  | otherwise = "0"  -- Handle negative as 0 for simplicity
  where go 0 ds = ds
        go x ds = go (x `div` 3) (("012" !! (x `mod` 3)) : ds)
