module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number xs =
    let digits = filter isDigit xs
        clean = case digits of
            ('1':rest) | length rest == 10 -> rest
            _ | length digits == 10 -> digits
            _ -> ""
    in if validNANP clean then Just clean else Nothing

-- NANP: NXX NXX-XXXX, N=2-9, X=0-9
validNANP :: String -> Bool
validNANP n
    | length n /= 10 = False
    | otherwise =
        let area = n !! 0
            exch = n !! 3
        in all (\c -> c >= '2' && c <= '9') [area, exch]
