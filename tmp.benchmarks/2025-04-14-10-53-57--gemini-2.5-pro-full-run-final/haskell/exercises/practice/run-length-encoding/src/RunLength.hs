module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, span)

-- | Decodes a run-length encoded string.
decode :: String -> String
decode [] = []
decode encodedText =
    let -- Split the string into the initial number part (if any) and the rest
        (numStr, rest) = span isDigit encodedText
    in case rest of
         -- If 'rest' is empty after the number, it's an invalid encoding or end of string
         [] -> ""
         -- Take the character following the number (or the first char if no number)
         (c:cs) ->
             -- Determine the repetition count. If numStr is empty, count is 1. Otherwise, read the number.
             let count = if null numStr then 1 else read numStr
             -- Replicate the character 'count' times and recursively decode the remainder of the string.
             in replicate count c ++ decode cs

-- | Encodes a string using run-length encoding.
encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    -- | Encodes a single group of identical characters.
    encodeGroup :: String -> String
    encodeGroup grp =
        let len = length grp
            char = head grp -- The character for this group
        in if len == 1
           -- If the length is 1, just output the character.
           then [char]
           -- If the length is greater than 1, output the length followed by the character.
           else show len ++ [char]
