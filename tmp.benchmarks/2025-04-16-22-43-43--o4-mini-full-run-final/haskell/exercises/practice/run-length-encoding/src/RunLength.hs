module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

-- | Decode a run-length–encoded string.
--   Digits specify the count for the following character; absence of digits means count = 1.
--   E.g. "2AB3CD4E" -> "AABCCCDEEEE"
decode :: String -> String
decode [] = []
decode xs =
  let (numStr, rest) = span isDigit xs
      count = if null numStr then 1 else read numStr
  in case rest of
       (c:cs) -> replicate count c ++ decode cs
       []     -> []  -- if there's a trailing digit sequence with no character, ignore

-- | Encode a string via run-length encoding.
--   Consecutive runs of the same character are replaced by "<count><char>" when count > 1.
--   E.g. "AABCCCDEEEE" -> "2AB3CD4E"
encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup grp =
      let n = length grp
          c = head grp
      in (if n > 1 then show n else "") ++ [c]
