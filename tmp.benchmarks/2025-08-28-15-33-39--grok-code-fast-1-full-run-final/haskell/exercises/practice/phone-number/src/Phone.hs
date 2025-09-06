module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number input =
  let digits = filter isDigit input
  in case length digits of
       10 -> if digits !! 0 `elem` ['2'..'9'] && digits !! 3 `elem` ['2'..'9'] then Just digits else Nothing
       11 -> if head digits == '1' then let d = tail digits in if d !! 0 `elem` ['2'..'9'] && d !! 3 `elem` ['2'..'9'] then Just d else Nothing else Nothing
       _ -> Nothing
