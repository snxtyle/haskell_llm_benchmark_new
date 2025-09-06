module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose ls = if length ls == 1 then customTranspose padded else map rstrip $ customTranspose padded
  where
    maxLen = maximum $ map length ls
    padded = map (\row -> row ++ replicate (maxLen - length row) ' ') ls
    customTranspose mat = [ [ mat !! j !! i | j <- [0..length mat - 1] ] | i <- [0..maxLen - 1] ]
    rstrip = reverse . dropWhile (== ' ') . reverse
