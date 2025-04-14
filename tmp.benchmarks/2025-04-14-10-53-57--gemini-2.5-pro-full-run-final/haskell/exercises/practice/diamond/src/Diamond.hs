module Diamond (diamond) where

import Data.Char (isUpper, ord, chr)

-- | Generates a diamond pattern for a given uppercase letter.
-- Returns Nothing if the input character is not an uppercase letter.
diamond :: Char -> Maybe [String]
diamond c
  | not (isUpper c) = Nothing -- Input must be an uppercase letter
  | otherwise       = Just (generateDiamond c)

-- | Helper function to generate the list of strings for the diamond.
generateDiamond :: Char -> [String]
generateDiamond c = topHalf ++ bottomHalf
  where
    -- Calculate the index 'n' of the character 'c' (0 for 'A', 1 for 'B', etc.)
    n = ord c - ord 'A'
    -- Generate all characters from 'A' up to the input character 'c'
    chars = ['A' .. c]
    -- Generate the top half of the diamond including the middle row
    topHalf = map (generateRow n) chars
    -- Generate the bottom half by reversing the top half (excluding the middle row)
    bottomHalf = reverse (init topHalf)

-- | Generates a single row of the diamond for a given letter 'l'.
-- 'n' is the index of the widest character (e.g., for 'C', n=2).
generateRow :: Int -> Char -> String
generateRow n l = outerPadding ++ content ++ outerPadding
  where
    -- Calculate the index 'i' of the current letter 'l'
    i = ord l - ord 'A'
    -- Calculate the number of outer spaces (leading/trailing)
    outerPadding = replicate (n - i) ' '
    -- Determine the content of the row between the outer padding
    content
      -- The row for 'A' has only one character
      | l == 'A'  = "A"
      -- Rows for other letters have two characters with inner padding
      | otherwise = [l] ++ innerPadding ++ [l]
    -- Calculate the number of inner spaces between the two letters
    innerPadding = replicate (2 * i - 1) ' '
