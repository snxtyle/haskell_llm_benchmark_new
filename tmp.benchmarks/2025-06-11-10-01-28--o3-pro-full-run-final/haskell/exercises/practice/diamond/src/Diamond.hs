module Diamond (diamond) where

import Data.Char (ord, chr)

-- | Construct a diamond made of letters from 'A' up to the supplied character.
--
-- The function follows the usual “diamond-kata” rules and returns
--   • Just rows when the input is an uppercase letter between ‘A’ and ‘Z’.
--   • Nothing for any other character.
--
-- Each row has equal leading and trailing spaces, so the produced list is a
-- square (width = height).
diamond :: Char -> Maybe [String]
diamond ch
  | ch < 'A' || ch > 'Z' = Nothing
  | otherwise            = Just $ topHalf ++ tail (reverse topHalf)
  where
    -- Distance from 'A'
    n :: Int
    n = ord ch - ord 'A'

    -- Rows from the top (index 0) down to the middle row (index n)
    topHalf :: [String]
    topHalf = [row i | i <- [0 .. n]]

    -- Construct a single row given its index i (0-based from the top)
    row :: Int -> String
    row i =
      let c     = chr (ord 'A' + i)
          outer = replicate (n - i) ' '
      in if i == 0
            then outer ++ [c] ++ outer
            else
              let inner = replicate (2 * i - 1) ' '
              in outer ++ (c : inner ++ [c]) ++ outer
