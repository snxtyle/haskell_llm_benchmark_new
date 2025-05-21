module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

-- | Encodes a string using run-length encoding.
--   For example: "AABCCCDEEEE" -> "2AB3CD4E"
encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup :: String -> String
    encodeGroup [] = []
    encodeGroup (c:cs) =
      let len = length (c:cs)
      in if len == 1
         then [c]
         else show len ++ [c]

-- | Decodes a run-length encoded string.
--   For example: "2AB3CD4E" -> "AABCCCDEEEE"
decode :: String -> String
decode encodedText = decode' encodedText 0 ""
  where
    -- Helper function for decoding
    -- Takes: remaining string, current accumulated count, current decoded string
    decode' :: String -> Int -> String -> String
    decode' [] count acc =
        -- If we reach the end of the string and there's a pending count,
        -- it means the input was malformed (number without a character).
        -- However, the problem statement implies valid input where numbers
        -- are always followed by a character. So, if count is 0, just return acc.
        acc
    decode' (x:xs) count acc
      | isDigit x =
          -- If it's a digit, accumulate the number
          decode' xs (count * 10 + (read [x] :: Int)) acc
      | otherwise =
          -- If it's a character, repeat it 'count' times (or 1 if count is 0)
          let actualCount = if count == 0 then 1 else count
          in decode' xs 0 (acc ++ replicate actualCount x)
