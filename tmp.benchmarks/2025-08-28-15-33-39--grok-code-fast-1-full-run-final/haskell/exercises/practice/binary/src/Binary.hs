module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs = if all (`elem` "01") xs
               then foldl (\acc c -> acc * 2 + (if c == '0' then 0 else 1)) 0 xs
               else 0
