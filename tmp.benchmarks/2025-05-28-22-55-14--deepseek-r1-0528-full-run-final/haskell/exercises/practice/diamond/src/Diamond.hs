module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c 
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (fullDiamond n)
  where
    n = ord c - ord 'A'

    fullDiamond :: Int -> [String]
    fullDiamond num = top ++ bottom
      where
        top = [ makeRow num i | i <- [0..num] ]
        bottom = reverse (init top)

    makeRow :: Int -> Int -> String
    makeRow num i
      | i == 0    = outerSpaces ++ [letter] ++ outerSpaces
      | otherwise = outerSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ outerSpaces
      where
        letter = chr (ord 'A' + i)
        outerSpaces = replicate (num - i) ' '
        innerSpaces = replicate (2*i - 1) ' '
