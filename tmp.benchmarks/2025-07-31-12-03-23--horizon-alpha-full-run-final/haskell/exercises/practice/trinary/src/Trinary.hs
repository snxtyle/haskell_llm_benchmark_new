module Trinary (readTri, showTri) where

-- Convert a trinary string to its decimal Integer value.
-- Invalid input (any char not in '0','1','2') yields 0.
readTri :: String -> Integer
readTri s
  | all isTriDigit s = foldl' step 0 s
  | otherwise        = 0
  where
    isTriDigit :: Char -> Bool
    isTriDigit c = c == '0' || c == '1' || c == '2'

    digitVal :: Char -> Integer
    digitVal '0' = 0
    digitVal '1' = 1
    digitVal '2' = 2
    digitVal _   = 0  -- unreachable due to validation

    step :: Integer -> Char -> Integer
    step acc c =
      let acc' = acc * 3 + digitVal c
      in acc' `seq` acc'

-- Convert a non-negative Integer to its trinary string representation.
-- For 0, return "0". For negative inputs, return "0".
showTri :: Integer -> String
showTri n
  | n <= 0    = "0"
  | otherwise = toTri n
  where
    toTri :: Integer -> String
    toTri x = go x []
      where
        go :: Integer -> [Char] -> [Char]
        go 0 acc = acc
        go y acc =
          let (q, r) = y `quotRem` 3
              ch     = case r of
                          0 -> '0'
                          1 -> '1'
                          _ -> '2'
              acc'   = ch : acc
          in acc' `seq` if q == 0 then acc' else go q acc'

-- Strict left fold implemented with Prelude only, using seq to force strictness
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z0 xs = go z0 xs
  where
    go acc []     = acc
    go acc (y:ys) =
      let acc' = f acc y
      in acc' `seq` go acc' ys
