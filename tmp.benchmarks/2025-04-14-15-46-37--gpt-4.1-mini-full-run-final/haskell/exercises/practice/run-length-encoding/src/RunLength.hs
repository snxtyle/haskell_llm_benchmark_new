module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode encodedText = decodeHelper encodedText ""
  where
    decodeHelper [] acc = reverse acc
    decodeHelper xs acc =
      let (countStr, rest) = span isDigit xs
          count = if null countStr then 1 else read countStr
      in case rest of
           (c:cs) -> decodeHelper cs (replicate count c ++ acc)
           [] -> reverse acc  -- no character after count, just return

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    group [] = []
    group (x:xs) = let (firstGroup, rest) = span (== x) xs
                   in (x:firstGroup) : group rest
    encodeGroup g@(x:_)
      | length g == 1 = [x]
      | otherwise = show (length g) ++ [x]
    encodeGroup [] = []
