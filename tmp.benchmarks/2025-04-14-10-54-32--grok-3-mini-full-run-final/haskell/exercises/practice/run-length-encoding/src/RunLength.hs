module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit)

encode :: String -> String
encode text = concatMap processGroup (group text)
  where
    processGroup xs
      | length xs > 1 = show (length xs) ++ [head xs]
      | otherwise     = [head xs]

decode :: String -> String
decode s = go s []
  where
    go [] acc = concat (reverse acc)  -- Reverse and concatenate the accumulated strings
    go str acc =
      case takeWhile isDigit str of
        [] ->  -- No digits, so take the first character
          let (x:xs) = str  -- Assuming the string is non-empty and well-formed
          in go xs ([x] : acc)
        digitsStr ->
          let num = read digitsStr :: Int  -- Convert digits to an integer
              restAfterDigits = drop (length digitsStr) str
          in if null restAfterDigits
             then error "Invalid input: incomplete encoding"  -- Handle edge case
             else
               let (c:rest) = restAfterDigits  -- Next character after digits
               in go rest (replicate num c : acc)  -- Add the repeated character to acc
