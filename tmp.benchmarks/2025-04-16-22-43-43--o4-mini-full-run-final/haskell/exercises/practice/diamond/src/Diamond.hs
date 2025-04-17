module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise          = Just (buildDiamond n)
  where
    -- n is the zero-based index of the target letter (A=0, B=1, …)
    n = ord c - ord 'A'

    -- Build the full diamond by concatenating the top and bottom halves
    buildDiamond :: Int -> [String]
    buildDiamond m =
      let top    = [makeLine k m | k <- [0 .. m]]
          bottom = [makeLine k m | k <- [m - 1, m - 2 .. 0]]
      in top ++ bottom

    -- Construct a single line for letter at index k, given max index m
    makeLine :: Int -> Int -> String
    makeLine k m =
      let letter       = chr (ord 'A' + k)
          outerSpaces  = replicate (m - k) ' '
          innerSpaces
            | k == 0    = ""
            | otherwise = replicate (2 * k - 1) ' '
      in if k == 0
         then outerSpaces ++ [letter] ++ outerSpaces
         else outerSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ outerSpaces
