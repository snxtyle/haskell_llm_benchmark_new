module Transpose (transpose) where

import qualified Data.List as L

-- Pad each string on the left with spaces to the length of the longest string
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

-- Remove trailing spaces from a string
rstrip :: String -> String
rstrip = reverse . dropWhile (== ' ') . reverse

transpose :: [String] -> [String]
transpose lines =
  let maxLen = maximum (map length lines)
      padded = map (padLeft maxLen) lines
      transposed = L.transpose padded
  in map rstrip transposed
