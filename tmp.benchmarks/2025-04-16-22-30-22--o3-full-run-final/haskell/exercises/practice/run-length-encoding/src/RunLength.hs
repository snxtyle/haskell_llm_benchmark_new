module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- | Perform run‑length encoding.
--   Consecutive runs of the same character are replaced by
--   the run length followed by the character. Runs of length 1
--   are left without a preceding count.
--
--   >>> encode "AABCCCDEEEE"
--   "2AB3CD4E"
encode :: String -> String
encode = concatMap encodeRun . group
  where
    encodeRun xs =
      let n  = length xs
          ch = head xs
       in (if n == 1 then "" else show n) ++ [ch]

-- | Decode a run‑length encoded string.
--
--   >>> decode "2AB3CD4E"
--   "AABCCCDEEEE"
decode :: String -> String
decode "" = ""
decode s  =
  let (digits, rest) = span isDigit s
      count          = if null digits then 1 else read digits
   in case rest of
        []       -> ""  -- This case should not occur with valid input.
        (c:cs)   -> replicate count c ++ decode cs
