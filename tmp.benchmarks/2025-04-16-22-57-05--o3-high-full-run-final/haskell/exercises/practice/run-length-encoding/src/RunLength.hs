module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- | Run‑length encode a string.
-- Consecutive identical characters are replaced by a single character
-- prefixed by the number of occurrences (omitted when the count is 1).
--
-- >>> encode "AABCCCDEEEE"
-- "2AB3CD4E"
encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup xs =
      let len = length xs
          c   = head xs
       in (if len == 1 then "" else show len) ++ [c]

-- | Decode a run‑length encoded string.
--
-- >>> decode "2AB3CD4E"
-- "AABCCCDEEEE"
decode :: String -> String
decode = go ""
  where
    -- 'digits' accumulates any preceding digit characters that make up the count
    go :: String -> String -> String
    go digits [] = []  -- end of input
    go digits (x : xs)
      | isDigit x = go (digits ++ [x]) xs
      | otherwise =
          let count = if null digits then 1 else read digits
           in replicate count x ++ go "" xs
