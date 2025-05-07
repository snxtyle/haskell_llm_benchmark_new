module Transpose (transpose) where

import Data.Maybe (isNothing, fromMaybe)

-- | Transpose a list of strings according to the rules described in the
--   exercise statement.
--
--   • Shorter input rows are conceptually padded on the right with “virtual”
--     spaces (Nothing).  
--   • When producing each output row we turn Nothings into actual spaces,
--     but remove trailing Nothings so that no extra spaces are appended.  
--   • Real spaces that are present in the input are kept.
transpose :: [String] -> [String]
transpose [] = []
transpose rows = map buildRow [0 .. maxLen - 1]
  where
    maxLen :: Int
    maxLen = maximum (0 : map length rows)

    buildRow :: Int -> String
    buildRow col = map (fromMaybe ' ') (trimTrailingNothings $ map (charAt col) rows)

    -- Get the character at the given column or Nothing if the row is too short.
    charAt :: Int -> String -> Maybe Char
    charAt idx str
      | idx < length str = Just (str !! idx)
      | otherwise        = Nothing

-- | Remove trailing 'Nothing's – these correspond to the virtual padding we
--   must not include at the right end of an output row.
trimTrailingNothings :: [Maybe a] -> [Maybe a]
trimTrailingNothings = reverse . dropWhile isNothing . reverse
