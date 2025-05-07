module Transpose (transpose) where

import Data.List (dropWhileEnd)

-- | Transpose a block of text represented as a list of lines.
--   If some lines are shorter than the longest line,
--   virtual spaces are inserted on the *left* (i.e. in the positions
--   that come before any real character of that line).
--   After the actual transposition we remove only the padding spaces
--   that ended up at the right‑hand side of a result line; this way
--   we never lose spaces that really existed in the source.
transpose :: [String] -> [String]
transpose [] = []
transpose rows = map buildColumn [0 .. maxLen - 1]
  where
    maxLen :: Int
    maxLen = maximum (0 : map length rows)

    -- For a given column index collect all characters (real or padded),
    -- then drop the trailing *padded* ones.
    buildColumn :: Int -> String
    buildColumn idx =
        let columnWithFlags = map (charAt idx) rows               -- [(Char, Bool)]
            -- Drop only the padding added for rows that were too short.
            trimmed          = dropWhileEnd (not . snd) columnWithFlags
        in  map fst trimmed

    -- Extract the character at the requested index from a row.
    -- If the row is too short we return a padding space and mark it as “not real”.
    charAt :: Int -> String -> (Char, Bool)
    charAt i row
        | i < length row = (row !! i, True)   -- real character
        | otherwise      = (' ',      False)  -- left‑pad space (virtual)
