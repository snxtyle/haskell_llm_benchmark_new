module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs = 
    if all (\x -> x == '0' || x == '1') xs
        then foldl (\acc (pos, digit) -> acc + if digit == '1' then 2 ^ pos else 0) 0 indexed
        else 0
    where indexed = zip [length xs - 1, length xs - 2 .. 0] xs
