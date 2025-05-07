module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

decode :: String -> String
decode = go ""
  where
    go :: String -> String -> String -- Accumulator for number string, remaining encoded text
    go numStr [] = [] -- If input ends, we are done. If numStr was not empty, it implies an invalid format like "12", which we ignore.
    go numStr (char:rest)
      | isDigit char = go (numStr ++ [char]) rest -- Append digit to numStr and continue
      | otherwise    = decodeChar numStr char ++ go "" rest -- Process character with accumulated numStr, then reset numStr

    decodeChar :: String -> Char -> String -- Accumulated number string, character to repeat
    decodeChar numStr charToRepeat
      | null numStr = [charToRepeat] -- No preceding number, so character appears once
      | otherwise   = replicate (read numStr) charToRepeat -- Repeat character 'read numStr' times

encode :: String -> String
encode [] = ""
encode text = concatMap processGroup (group text)
  where
    processGroup :: String -> String -- A group of identical characters, e.g., "AAA"
    processGroup groupChars =
      let
        char = head groupChars
        len = length groupChars
      in
        if len == 1
          then [char] -- Single character, no count prefix
          else show len ++ [char] -- Count prefix for runs longer than 1
