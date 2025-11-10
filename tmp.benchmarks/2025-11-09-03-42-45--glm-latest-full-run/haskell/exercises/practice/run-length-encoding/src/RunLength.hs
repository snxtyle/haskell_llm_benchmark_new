module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

-- | Run-length encode a string.
-- Consecutive identical characters are replaced by a count and the character.
-- The count is omitted if it is 1.
-- >>> encode "AAAABBBCCDAA"
-- "4A3B2C1D2A"
-- >>> encode "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
-- "12WB12W3B24WB"
encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup cs@(c:_) =
        let len = length cs
        in if len == 1 then [c] else show len ++ [c]
    encodeGroup [] = [] -- This case should not be reached with `group`

-- | Decode a run-length encoded string.
-- Parses counts and characters to reconstruct the original string.
-- >>> decode "4A3B2C1D2A"
-- "AAAABBBCCDAA"
-- >>> decode "12WB12W3B24WB"
-- "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
decode :: String -> String
decode = go
  where
    go :: String -> String
    go [] = []
    go (c:cs)
        | isDigit c =
            let (numStr, rest) = span isDigit (c:cs)
                count = read numStr
                (char:restOfRest) = rest
            in replicate count char ++ go restOfRest
        | otherwise = c : go cs
