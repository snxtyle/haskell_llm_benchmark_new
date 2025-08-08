module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn s =
  let cleaned = filter (/= '-') s
  in if length cleaned /= 10
       then False
       else
         let first9 = take 9 cleaned
             lastCh  = cleaned !! 9
         in  all isDigit first9
             && validLast lastCh
             && (calcSum first9 lastCh `mod` 11 == 0)
  where
    validLast :: Char -> Bool
    validLast c = isDigit c || c == 'X'

    calcSum :: String -> Char -> Int
    calcSum digits lastCh =
      let weights = [10,9..2]
          sum9    = sum (zipWith (*) (map digitToInt digits) weights)
          lastVal = if lastCh == 'X' then 10 else digitToInt lastCh
      in sum9 + lastVal
