module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt, toUpper)

isbn :: String -> Bool
isbn s = 
  let clean = filter (/= '-') (map toUpper s)
  in case clean of
      [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10] -> 
        let digits = [d1,d2,d3,d4,d5,d6,d7,d8,d9]
            check = d10
        in if all isDigit digits && (check == 'X' || isDigit check)
           then 
             let nums = map digitToInt digits
                 checkVal = if check == 'X' then 10 else digitToInt check
                 weights = [10,9..1]
                 total = sum $ zipWith (*) (nums ++ [checkVal]) weights
             in total `mod` 11 == 0
           else False
      _ -> False
