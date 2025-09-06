module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs = 
    let digits = filter isDigit xs
        cleaned = case digits of
            '1':rest | length rest == 10 -> rest
            other | length other == 10 -> other
            _ -> ""
    in if isValid cleaned then Just cleaned else Nothing
    where
        isValid :: String -> Bool
        isValid num = 
            length num == 10 && 
            num !! 0 `elem` ['2'..'9'] && 
            num !! 3 `elem` ['2'..'9']
