module RunLength (decode, encode) where

import Data.Char (isDigit, digitToInt)
import Data.List (group)

-- | Run-length encode a string.
--   Consecutive runs of the same character are replaced by the run length
--   followed by the character.  A run length of 1 is omitted.
--
--   >>> encode "AABCCCDEEEE"
--   "2AB3CD4E"
encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup xs =
      let len = length xs
          prefix = if len == 1 then "" else show len
      in prefix ++ [head xs]

-- | Decode a run-length encoded string produced by ‘encode’.
--
--   >>> decode "2AB3CD4E"
--   "AABCCCDEEEE"
decode :: String -> String
decode = go id 0
  where
    -- Difference-list accumulator for efficiency.
    go :: ([Char] -> [Char]) -> Int -> String -> String
    go acc n [] = acc []
    go acc n (c:cs)
      | isDigit c =
          -- Accumulate multi-digit counts (e.g. "12")
          go acc (n * 10 + digitToInt c) cs
      | otherwise =
          -- Non-digit marks the end of the count (if any); emit the run
          let cnt = if n == 0 then 1 else n
              acc' = acc . (replicate cnt c ++)
          in go acc' 0 cs
