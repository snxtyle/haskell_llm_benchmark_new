module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode = decodeHelper ""
  where
    decodeHelper :: String -> String -> String
    decodeHelper acc [] = acc
    decodeHelper acc str =
      let (numStr, rest) = span isDigit str
      in case rest of
           [] -> acc
           (c:cs) ->
             let count = if null numStr then 1 else read numStr
             in decodeHelper (acc ++ replicate count c) cs

encode :: String -> String
encode text = concatMap encodeGroup (group text)
  where
    encodeGroup :: String -> String
    encodeGroup [] = ""
    encodeGroup grp@(c:_) =
      let len = length grp
      in if len == 1
         then [c]
         else show len ++ [c]
