module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode s@(x:xs)
  | isDigit x =
      let (numStr, rest) = span isDigit s
      in case rest of
           -- Invalid format: digits at the end of the string.
           -- We'll treat this as an empty result for that part.
           [] -> ""
           (c:cs) -> replicate (read numStr) c ++ decode cs
  | otherwise = x : decode xs

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup :: String -> String
    encodeGroup s@(c:_) =
      let len = length s
      in if len > 1
         then show len ++ [c]
         else [c]

