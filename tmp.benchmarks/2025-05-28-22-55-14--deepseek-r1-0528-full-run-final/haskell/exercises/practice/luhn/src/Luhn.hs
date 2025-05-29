module Luhn (isValid) where

import Data.Char (isDigit, isSpace, digitToInt)

isValid :: String -> Bool
isValid n = 
  let clean = filter (not . isSpace) n
      len   = length clean
  in if len <= 1 || any (not . isDigit) clean
     then False
     else 
        let digits = map digitToInt clean
            processed = [ if (len - i) `mod` 2 == 0 
                          then let doubled = 2 * d in if doubled > 9 then doubled - 9 else doubled 
                          else d 
                        | (i, d) <- zip [0..] digits ]
        in (sum processed) `mod` 10 == 0
