module Diamond (diamond) where

import Data.Char (ord, chr)
import Data.List (intercalate)

diamond :: Char -> Maybe [String]
diamond ch
  | ch < 'A' || ch > 'Z' = Nothing
  | otherwise = Just (topHalf ++ bottomHalf)
  where
    n = ord ch - ord 'A'
    topHalf = [makeRow i | i <- [0..n]]
    bottomHalf = reverse (init topHalf)  -- Reverse top half (excluding the middle row)
    
    makeRow i = 
      let letter = chr (ord 'A' + i)
          outerSpaces = replicate (n - i) ' '
          innerSpaces = replicate (2 * i - 1) ' '
          row = if i == 0 
                then [letter]  -- Special case for 'A'
                else [letter] ++ innerSpaces ++ [letter]
      in outerSpaces ++ row ++ outerSpaces
