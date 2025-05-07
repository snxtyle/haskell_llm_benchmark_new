module Diamond (diamond) where

import Data.Char (ord, chr)

-- | Builds a diamond starting with 'A' and widest at the supplied character.
--   Returns Nothing for characters outside the range 'A'‑'Z'.
--
--   Each line keeps the required leading and trailing spaces so that
--   the resulting list of strings has a perfect square shape.
diamond :: Char -> Maybe [String]
diamond c
  | 'A' <= c && c <= 'Z' = Just $ map mkLine indices
  | otherwise            = Nothing
  where
    -- Distance of the supplied letter from 'A'
    n :: Int
    n = ord c - ord 'A'

    -- Indices for the top half (ascending) and bottom half (descending)
    indices :: [Int]
    indices = [0 .. n] ++ [n - 1, n - 2 .. 0]

    -- Construct a single line of the diamond for the given index.
    mkLine :: Int -> String
    mkLine i =
      let padding = replicate (n - i) ' '
          letter  = chr (ord 'A' + i)
          middle  = if i == 0
                      then ""                          -- Single 'A' line
                      else replicate (2 * i - 1) ' '  -- Spaces between duplicate letters
      in padding ++ [letter] ++ middle ++ (if i == 0 then "" else [letter]) ++ padding
