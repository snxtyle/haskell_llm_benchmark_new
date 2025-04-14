module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let digitsOnly = filter isDigit xs  -- Remove non-digit characters
        processNumber ds
          | length ds == 11 && head ds == '1' = drop 1 ds  -- Remove leading '1' if present
          | otherwise = ds
        cleaned = processNumber digitsOnly
    in if length cleaned == 10 && isValidNANP cleaned
       then Just cleaned
       else Nothing
  where
    isValidNANP ds = 
        -- Check if the string has exactly 10 characters and all are digits
        length ds == 10 && all isDigit ds &&
        -- 1st digit (area code start): 2-9
        (head ds >= '2' && head ds <= '9') &&
        -- 4th digit (exchange code start): 2-9
        (ds !! 3 >= '2' && ds !! 3 <= '9')
        -- No need to check other digits as they can be 0-9
