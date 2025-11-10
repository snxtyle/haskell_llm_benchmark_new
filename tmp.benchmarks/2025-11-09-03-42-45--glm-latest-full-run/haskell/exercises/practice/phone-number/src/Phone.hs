module Phone (number) where

import Data.Char (isDigit)

-- | Cleans up a user-entered phone number string.
-- Returns 'Nothing' if the number is invalid, otherwise 'Just' the cleaned 10-digit number.
number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
        -- Remove leading country code '1' if present
        tenDigits = case length digits of
                      11 -> if head digits == '1' then tail digits else ""
                      10 -> digits
                      _  -> "" -- Invalid length
    in if length tenDigits /= 10
       then Nothing
       else
           -- Validate the 10-digit number
           let areaCode   = tenDigits !! 0
               exchangeCode = tenDigits !! 3
               -- N is a digit from 2 through 9
               isValidN d = d `elem` "23456789"
           in if isValidN areaCode && isValidN exchangeCode
              then Just tenDigits
              else Nothing
