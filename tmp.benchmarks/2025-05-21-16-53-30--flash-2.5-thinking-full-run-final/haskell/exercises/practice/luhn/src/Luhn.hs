module Luhn (isValid) where

import Data.Char (isDigit, digitToInt)
import Data.List (reverse)

isValid :: String -> Bool
isValid n =
    let
        -- 1. Clean the input: remove spaces
        cleanedN = filter (/= ' ') n
        len = length cleanedN
    in
    -- 2. Validate cleanedN:
    --    - Length 1 or less is invalid
    --    - Contains non-digit characters is invalid
    if len <= 1 || any (not . isDigit) cleanedN
    then False
    else
        let
            -- 3. Convert to digits
            digits = map digitToInt cleanedN
            -- 4. Apply Luhn transformation:
            --    - Reverse the list
            --    - Double every second digit (odd index in reversed list)
            --    - Subtract 9 if doubled digit > 9
            transformedDigits =
                zipWith (\idx d ->
                    if odd idx
                    then let doubled = d * 2
                         in if doubled > 9 then doubled - 9 else doubled
                    else d
                ) [0..] (reverse digits)
            -- 5. Sum and check divisibility by 10
            totalSum = sum transformedDigits
        in
        totalSum `mod` 10 == 0
