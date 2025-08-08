module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn input =
  let filtered = filter (/= '-') input
  in if length filtered /= 10
       then False
       else
         let (first9, rest) = splitAt 9 filtered
         in case rest of
              [lastCh]
                | all isDigit first9
                , isDigit lastCh || lastCh == 'X' ->
                    let digits = map digitToInt first9 ++ [if lastCh == 'X' then 10 else digitToInt lastCh]
                        weights = [10,9..1]
                        total = sum (zipWith (*) weights digits)
                    in total `mod` 11 == 0
                | otherwise -> False
              _ -> False
