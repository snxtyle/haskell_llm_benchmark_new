module IsbnVerifier (isbn) where

import Data.Char (isDigit, ord)

isbn :: String -> Bool
isbn s = let cleaned = filter (/= '-') s
         in length cleaned == 10 &&
            let ds = init cleaned
                ck = last cleaned
            in all isDigit ds &&
               (isDigit ck || ck == 'X') &&
               let digitVals = map (\c -> ord c - ord '0') ds
                   checkVal = if ck == 'X' then 10 else ord ck - ord '0'
                   vals = digitVals ++ [checkVal]
                   weights = [10,9..1]
                   total = sum $ zipWith (*) vals weights
               in total `mod` 11 == 0
