module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)
import Data.List (span)

encode :: String -> String
encode = concatMap encodeGroup . group
  where
    encodeGroup xs
      | length xs == 1 = [head xs]
      | otherwise      = show (length xs) ++ [head xs]

decode :: String -> String
decode "" = ""
decode xs = decode' xs
  where
    decode' "" = ""
    decode' s =
      let (digits, rest) = span isDigit s
      in case rest of
           [] -> ""
           (c:cs) ->
             let count = if null digits then 1 else read digits
             in replicate count c ++ decode' cs
