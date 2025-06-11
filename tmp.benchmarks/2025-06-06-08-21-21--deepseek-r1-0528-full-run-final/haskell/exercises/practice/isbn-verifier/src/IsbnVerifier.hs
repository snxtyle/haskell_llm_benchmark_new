module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn s = 
    let clean = filter (/= '-') s
    in if length clean /= 10 
        then False
        else let first9 = take 9 clean
                 lastChar = last clean
             in if not (all isDigit first9)
                then False
                else if lastChar `notElem` "0123456789Xx"
                     then False
                     else let digits = map digitToInt first9 
                              lastVal = if lastChar `elem` ['X','x'] then 10 else digitToInt lastChar
                              allDigits = digits ++ [lastVal]
                              weights = [10,9..1]
                              total = sum $ zipWith (*) allDigits weights
                          in total `mod` 11 == 0
