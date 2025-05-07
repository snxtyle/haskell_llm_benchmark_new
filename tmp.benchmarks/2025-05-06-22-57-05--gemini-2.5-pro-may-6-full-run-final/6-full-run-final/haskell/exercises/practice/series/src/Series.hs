module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n xs
    | n < 0     = []  -- Negative length is invalid, return empty list.
    | n == 0    = replicate (length xs + 1) []  -- For n=0, result is L+1 empty lists.
    | otherwise = -- Case for n > 0
        let digits = map digitToInt xs
            numDigits = length digits
            -- Calculate how many slices of length 'n' can be formed.
            -- If n > numDigits, numSlices will be <= 0.
            numSlices = numDigits - n + 1
        in
        -- If numSlices is not positive, `take numSlices` will correctly produce an empty list.
        -- Otherwise, it takes the relevant tails from which slices will be formed.
        map (take n) $ take numSlices $ tails digits
