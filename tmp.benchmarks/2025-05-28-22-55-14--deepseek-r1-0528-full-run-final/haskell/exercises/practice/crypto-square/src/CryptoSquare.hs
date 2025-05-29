module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)

encode :: String -> String
encode xs = 
    let normalized = map toLower $ filter isAlphaNum xs
        n = length normalized
        r0 = floor (sqrt (fromIntegral n))
        (r, c) = if r0 * r0 >= n
                 then (r0, r0)
                 else if r0 * (r0+1) >= n
                      then (r0, r0+1)
                      else (r0+1, r0+1)
        total = r * c
        padded = normalized ++ replicate (total - n) ' '
        rows = [ take c (drop (i*c) padded) | i <- [0..r-1] ]
        cols = [ [ row !! j | row <- rows ] | j <- [0..c-1] ]
    in unwords cols
