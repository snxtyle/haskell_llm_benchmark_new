module Phone (number) where

import Data.Char (isDigit)

-- | Cleans up a user-entered phone number according to NANP rules.
--   Returns Just the 10-digit number if valid, Nothing otherwise.
number :: String -> Maybe String
number xs =
    let
        -- 1. Filter out non-digit characters
        digitsOnly = filter isDigit xs
        len = length digitsOnly
    in
        case len of
            10 -> validateNumber digitsOnly
            11 ->
                -- Check for valid country code '1'
                if head digitsOnly == '1'
                then validateNumber (tail digitsOnly)
                else Nothing -- 11 digits but doesn't start with '1'
            _ -> Nothing -- Any other length is invalid

-- | Helper function to validate the 10-digit number format (NXX NXX-XXXX).
--   N is any digit from 2 through 9. X is any digit from 0 through 9.
validateNumber :: String -> Maybe String
validateNumber numStr
    | length numStr /= 10 = Nothing -- Should already be 10, but good for safety
    | head numStr `elem` ['2'..'9'] && numStr !! 3 `elem` ['2'..'9'] = Just numStr
    | otherwise = Nothing
